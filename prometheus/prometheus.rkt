#lang racket
(require racket/generic
         (for-syntax racket racket/syntax)
         web-server/http/response-structs)
(provide (except-out (struct-out prometheus-registry) -make-prometheus-registry)
         (except-out (struct-out prometheus-metric-counter) -make-prometheus-metric-counter)
         (except-out (struct-out prometheus-metric-gauge) -make-prometheus-metric-gauge)
         (except-out (struct-out prometheus-metric-histogram) -make-prometheus-metric-histogram)
         make-prometheus-registry
         current-prometheus-registry
         with-prometheus-registry
         make-prometheus-metric-counter
         make-prometheus-metric-gauge
         make-prometheus-metric-histogram
         prometheus-register!
         prometheus-unregister!
         prometheus-set!
         prometheus-reset!
         prometheus-increment!
         prometheus-decrement!
         prometheus-observe!
         prometheus-ref
         prometheus-metric-type
         prometheus-metric-name
         prometheus-metric-doc
         prometheus-metric-labels
         prometheus-metric-store
         prometheus-metric-value
         prometheus-metric-format
         prometheus-metrics-format
         prometheus-start-runtime-collector
         make-prometheus-http-handler)
(module+ test
  (require rackunit))

;;

(define-struct prometheus-registry
  ((metrics #:mutable) namespace labels semaphore)
  #:transparent
  #:constructor-name -make-prometheus-registry)

(define (make-prometheus-registry
         #:namespace (namespace #f)
         #:labels (labels null))
  (-make-prometheus-registry (make-hash) namespace labels
                             (make-semaphore 1)))

(define current-prometheus-registry
  (make-parameter (make-prometheus-registry)))

(define-syntax (with-prometheus-registry stx)
  (syntax-case stx ()
    ((_ registry body ...)
     (syntax/loc stx
       (call-with-semaphore
        (prometheus-registry-semaphore registry)
        (thunk body ...))))))

;;

(define (prometheus-metric-format-name name #:namespace (namespace #f))
  (if namespace
      (string-append namespace "_" name)
      name))

(define (prometheus-metric-label-format label)
  (format "~a=\"~a\"" (car label) (cdr label)))

(define (prometheus-metric-labels-format labels)
  (if (null? labels) ""
      (string-append "{" (string-join
                          (map prometheus-metric-label-format labels)
                          ",") "}")))

(define (prometheus-metric-meta-format name type doc)
  (format "~a# TYPE ~a ~a\n"
          (if doc (format "# HELP ~a ~a\n" name doc) "")
          name type))

(define (prometheus-metric-format-fields name type doc labels values)
  (for/fold ((acc (prometheus-metric-meta-format name type doc)))
            (((value-labels value) (in-hash values)))
    (string-append acc
                   (format "~a~a ~a\n"
                           name
                           (prometheus-metric-labels-format (append labels value-labels))
                           value))))

(define (prometheus-metrics-format #:registry (registry (current-prometheus-registry)))
  (for/fold ((acc ""))
            (((name metric) (in-hash (prometheus-registry-metrics registry))))
    (let* ((namespace (prometheus-registry-namespace registry)))
      (string-append acc (prometheus-metric-format metric #:namespace namespace)))))

(define-syntax (define-prometheus-metric stx)
  (syntax-case stx ()
    ((_ sym ((field init) ...) rest ...)
     (with-syntax ((public-ctor (format-id #'sym "make-~a" #'sym))
                   (private-ctor (format-id #'sym "-make-~a" #'sym)))
       (syntax/loc stx
         (begin
           (define-struct sym
             (name doc labels store semaphore field ...)
             #:constructor-name private-ctor
             rest ...)
           (define (public-ctor name
                                #:doc (doc #f)
                                #:labels (labels null))
             (private-ctor (if (symbol? name)
                               (symbol->string name)
                               name)
                           doc labels
                           (make-hash) (make-semaphore 1)
                           init ...))))))))

;;

(define-generics prometheus-metric
  (prometheus-metric-type prometheus-metric)
  (prometheus-metric-name prometheus-metric)
  (prometheus-metric-doc prometheus-metric)
  (prometheus-metric-labels prometheus-metric)
  (prometheus-metric-store prometheus-metric)
  (prometheus-metric-value prometheus-metric (default) #:labels (labels))
  (prometheus-metric-format prometheus-metric #:namespace (namespace))

  (prometheus-set! prometheus-metric value #:labels (labels))
  (prometheus-reset! prometheus-metric #:labels (labels))
  (prometheus-increment! prometheus-metric #:by (by) #:labels (labels))
  (prometheus-decrement! prometheus-metric #:by (by) #:labels (labels))
  (prometheus-observe! prometheus-metric value #:labels (labels)))

;;

(define-prometheus-metric prometheus-metric-counter ()
  #:methods gen:prometheus-metric
  ((define (prometheus-metric-type metric)
     'counter)
   (define (prometheus-metric-name metric)
     (prometheus-metric-counter-name metric))
   (define (prometheus-metric-doc metric)
     (prometheus-metric-counter-doc metric))
   (define (prometheus-metric-labels metric)
     (prometheus-metric-counter-labels metric))
   (define (prometheus-metric-store metric)
     (prometheus-metric-counter-store metric))
   (define (prometheus-metric-value metric (default #f) #:labels (labels null))
     (hash-ref (prometheus-metric-counter-store metric) labels default))
   (define (prometheus-metric-format metric #:namespace (namespace #f))
     (prometheus-metric-format-fields
      (prometheus-metric-format-name (prometheus-metric-name metric)
                                     #:namespace namespace)
      (prometheus-metric-type metric)
      (prometheus-metric-doc metric)
      (prometheus-metric-labels metric)
      (prometheus-metric-store metric)))

   (define (prometheus-increment! metric
                                  #:by (by 1)
                                  #:labels (labels null))
     (let ((store (prometheus-metric-counter-store metric)))
       (call-with-semaphore
        (prometheus-metric-counter-semaphore metric)
        (thunk (let ((new-value (+ by (hash-ref store labels 0))))
                 (begin0 new-value
                   (hash-set! store labels new-value)))))))))

;;

(define-prometheus-metric prometheus-metric-gauge ()
  #:methods gen:prometheus-metric
  ((define (prometheus-metric-type metric)
     'gauge)
   (define (prometheus-metric-name metric)
     (prometheus-metric-gauge-name metric))
   (define (prometheus-metric-doc metric)
     (prometheus-metric-gauge-doc metric))
   (define (prometheus-metric-labels metric)
     (prometheus-metric-gauge-labels metric))
   (define (prometheus-metric-store metric)
     (prometheus-metric-gauge-store metric))
   (define (prometheus-metric-value metric (default #f) #:labels (labels null))
     (hash-ref (prometheus-metric-gauge-store metric) labels default))
   (define (prometheus-metric-format metric #:namespace (namespace #f))
     (prometheus-metric-format-fields
      (prometheus-metric-format-name (prometheus-metric-name metric)
                                     #:namespace namespace)
      (prometheus-metric-type metric)
      (prometheus-metric-doc metric)
      (prometheus-metric-labels metric)
      (prometheus-metric-store metric)))

   (define (prometheus-set! metric value #:labels (labels null))
     (let ((store (prometheus-metric-gauge-store metric)))
       (begin0 value
         (call-with-semaphore
          (prometheus-metric-gauge-semaphore metric)
          (thunk (hash-set! store labels value))))))
   (define (prometheus-reset! metric #:labels (labels null))
     (prometheus-set! metric 0 #:labels labels))
   (define (prometheus-increment! metric
                                  #:by (by 1)
                                  #:labels (labels null))
     (let ((store (prometheus-metric-gauge-store metric)))
       (call-with-semaphore
        (prometheus-metric-gauge-semaphore metric)
        (thunk (let ((new-value (+ by (hash-ref store labels 0))))
                 (begin0 new-value
                   (hash-set! store labels new-value)))))))
   (define (prometheus-decrement! metric
                                  #:by (by -1)
                                  #:labels (labels null))
     (prometheus-increment! metric
                            #:by by
                            #:labels labels))))

;;

(define-struct prometheus-metric-histogram-store-value
  (buckets sum count)
  #:transparent
  #:mutable)

(define-prometheus-metric prometheus-metric-histogram
  ((buckets (vector 0.005 0.01 0.025 0.05 0.1 0.25 0.5 1 2.5 5 10 +inf.0)))
  #:methods gen:prometheus-metric
  ((define (prometheus-metric-type metric)
     'histogram)
   (define (prometheus-metric-name metric)
     (prometheus-metric-histogram-name metric))
   (define (prometheus-metric-doc metric)
     (prometheus-metric-histogram-doc metric))
   (define (prometheus-metric-labels metric)
     (prometheus-metric-histogram-labels metric))
   (define (prometheus-metric-store metric)
     (prometheus-metric-histogram-store metric))
   (define (prometheus-metric-value metric (default #f) #:labels (labels null))
     (hash-ref (prometheus-metric-histogram-store metric) labels default))
   (define (prometheus-metric-format metric #:namespace (namespace #f))
     (let* ((name (prometheus-metric-format-name (prometheus-metric-name metric)
                                                 #:namespace namespace))
            (type (prometheus-metric-type metric))
            (doc (prometheus-metric-doc metric))
            (labels (prometheus-metric-labels metric))
            (store (prometheus-metric-store metric))
            (buckets (prometheus-metric-histogram-buckets metric))
            (meta-string (prometheus-metric-meta-format name type doc)))
       (for/fold ((acc meta-string))
                 (((value-labels value) (in-hash store)))
         (let* ((metric-labels (append labels value-labels))
                (metric-labels-string (prometheus-metric-labels-format metric-labels))
                (bucket-values (prometheus-metric-histogram-store-value-buckets value))
                (bucket-sum (prometheus-metric-histogram-store-value-sum value))
                (bucket-count (prometheus-metric-histogram-store-value-count value)))
           (set! acc
             (string-append acc
                            (format "~a_sum~a ~a\n"
                                    name
                                    metric-labels-string
                                    bucket-sum)))
           (set! acc
             (string-append acc
                            (format "~a_count~a ~a\n"
                                    name
                                    metric-labels-string
                                    bucket-count)))
           (for/fold ((acc acc))
                     ((bucket-value (in-vector bucket-values))
                      (index (in-naturals)))
             (string-append
              acc
              (format "~a_bucket~a ~a\n"
                      name
                      (prometheus-metric-labels-format
                       (cons `(le . ,(let ((le (vector-ref buckets index)))
                                       (if (= le +inf.0)
                                           "+Inf"
                                           (number->string le))))
                             metric-labels))
                      bucket-value)))))))

   (define (prometheus-observe! metric value
                                #:labels (labels null))
     (let ((store (prometheus-metric-histogram-store metric))
           (buckets (prometheus-metric-histogram-buckets metric)))
       (call-with-semaphore
        (prometheus-metric-histogram-semaphore metric)
        (thunk (let ((store-value (or (hash-ref store labels #f)
                                      (let ((value (make-prometheus-metric-histogram-store-value
                                                    (make-vector (vector-length buckets) 0)
                                                    0 0)))
                                        (begin0 value
                                          (hash-set! store labels value))))))
                 (begin0 store-value
                   (set-prometheus-metric-histogram-store-value-sum!
                    store-value
                    (+ value
                       (prometheus-metric-histogram-store-value-sum
                        store-value)))
                   (set-prometheus-metric-histogram-store-value-count!
                    store-value
                    (+ 1
                       (prometheus-metric-histogram-store-value-count
                        store-value)))
                   (let ((store-value-buckets (prometheus-metric-histogram-store-value-buckets store-value)))
                     (for ((upper-limit (in-vector buckets))
                           (index (in-naturals)))
                       (when (<= value upper-limit)
                         (vector-set! store-value-buckets index
                                      (+ 1 (vector-ref store-value-buckets index))))))))))))))

;;

(define -prometheus-ref-default (gensym))
(define (prometheus-ref name (default -prometheus-ref-default)
                        #:registry (registry (current-prometheus-registry)))
  (let ((value (with-prometheus-registry registry
                 (hash-ref (prometheus-registry-metrics registry)
                           (if (symbol? name)
                               (symbol->string name)
                               name)
                           default))))
    (when (eq? value -prometheus-ref-default)
      (error (format "no metric named ~s in registry" name)))
    value))

;;

(define (prometheus-register! metric #:registry (registry (current-prometheus-registry)))
  (begin0 metric
    (with-prometheus-registry registry
      (let ((metrics (prometheus-registry-metrics registry))
            (name (prometheus-metric-name metric)))
        (when (hash-ref metrics name #f)
          (error (format "metric ~s already exists" name)))
        (hash-set! metrics name metric)))))

(define (prometheus-unregister! metric #:registry (registry (current-prometheus-registry)))
  (begin0 metric
    (with-prometheus-registry registry
      (let ((metrics (prometheus-registry-metrics registry))
            (name (prometheus-metric-name metric)))
        (unless (hash-ref metrics name #f)
          (error (format "metric ~s does not exists" name)))
        (hash-remove! metrics name)))))

;;

(module+ test
  (test-case "make-prometheus-registry"
    (let ((registry (make-prometheus-registry)))
      (check-equal? (prometheus-registry-metrics registry)
                    (make-hash))
      (check-equal? (prometheus-registry-namespace registry)
                    #f)
      (check-equal? (prometheus-registry-labels registry)
                    null))
    (let ((registry (make-prometheus-registry
                     #:namespace "test"
                     #:labels '((a . b) (c . d)))))
      (check-equal? (prometheus-registry-metrics registry)
                    (make-hash))
      (check-equal? (prometheus-registry-namespace registry)
                    "test")
      (check-equal? (prometheus-registry-labels registry)
                    '((a . b) (c . d)))))
  (test-case "prometheus-register!"
    (parameterize ((current-prometheus-registry (make-prometheus-registry)))
      (check-equal? (prometheus-registry-metrics (current-prometheus-registry))
                    (make-hash))
      (let ((metric (make-prometheus-metric-counter 'foo)))
        (prometheus-register! metric)
        (check-equal? (prometheus-registry-metrics (current-prometheus-registry))
                      (make-hash `(("foo" . ,metric)))))))
  (test-case "prometheus-ref"
    (parameterize ((current-prometheus-registry (make-prometheus-registry)))
      (let ((metric (make-prometheus-metric-counter 'foo)))
        (prometheus-register! metric)
        (check-equal? (prometheus-ref 'foo) metric))))
  (test-case "prometheus-unregister!"
    (parameterize ((current-prometheus-registry (make-prometheus-registry)))
      (let ((metric (make-prometheus-metric-counter 'foo)))
        (prometheus-register! metric)
        (prometheus-unregister! metric))
      (check-equal? (prometheus-registry-metrics (current-prometheus-registry))
                    (make-hash))))
  (test-case "prometheus-label-format"
    (check-equal? (prometheus-metric-label-format (cons "test" 1))
                  "test=\"1\"")
    (check-equal? (prometheus-metric-label-format (cons 'test 1))
                  "test=\"1\"")
    (check-equal? (prometheus-metric-label-format (cons 'test 1.2))
                  "test=\"1.2\"")
    (check-equal? (prometheus-metric-label-format (cons 'test "hello"))
                  "test=\"hello\"")
    (check-equal? (prometheus-metric-label-format (cons 'test 'hello))
                  "test=\"hello\""))
  (test-case "prometheus-metric-labels-format"
    (check-equal? (prometheus-metric-labels-format '((1 . 2) (3 . 4)))
                  "{1=\"2\",3=\"4\"}"))
  (test-case "prometheus-metrics-format"
    (check-equal? (parameterize ((current-prometheus-registry (make-prometheus-registry
                                                               #:namespace "test")))
                    (let ((metric (make-prometheus-metric-counter 'foo #:doc "test")))
                      (prometheus-register! metric)
                      (prometheus-increment! metric #:labels '((c . d))))
                    (prometheus-metrics-format))
                  (string-join (list "# HELP test_foo test"
                                     "# TYPE test_foo counter"
                                     "test_foo{c=\"d\"} 1"
                                     "")
                               "\n"))
    (check-equal? (parameterize ((current-prometheus-registry (make-prometheus-registry)))
                    (let ((metric (make-prometheus-metric-counter 'foo #:doc "test")))
                      (prometheus-register! metric)
                      (prometheus-increment! metric #:labels '((c . d)))
                      (prometheus-increment! metric #:labels '((c . d)))
                      (prometheus-increment! metric #:labels '((c . f))))
                    (let ((metric (make-prometheus-metric-counter 'bar #:labels '((a . b)))))
                      (prometheus-register! metric)
                      (prometheus-increment! metric #:labels '((c . d))))
                    (prometheus-metrics-format))
                  (string-join (list "# TYPE bar counter"
                                     "bar{a=\"b\",c=\"d\"} 1"
                                     "# HELP foo test"
                                     "# TYPE foo counter"
                                     "foo{c=\"f\"} 1"
                                     "foo{c=\"d\"} 2"
                                     "")
                               "\n"))
    (check-equal? (parameterize ((current-prometheus-registry (make-prometheus-registry)))
                    (let ((metric (make-prometheus-metric-histogram 'foo #:doc "test")))
                      (prometheus-register! metric)
                      (prometheus-observe! metric 1 #:labels '((c . d)))
                      (prometheus-observe! metric 10 #:labels '((c . d)))
                      (prometheus-observe! metric 100 #:labels '((c . f))))
                    (prometheus-metrics-format))
                  (string-join (list "# HELP foo test"
                                     "# TYPE foo histogram"
                                     "foo_sum{c=\"f\"} 100"
                                     "foo_count{c=\"f\"} 1"
                                     "foo_bucket{le=\"0.005\",c=\"f\"} 0"
                                     "foo_bucket{le=\"0.01\",c=\"f\"} 0"
                                     "foo_bucket{le=\"0.025\",c=\"f\"} 0"
                                     "foo_bucket{le=\"0.05\",c=\"f\"} 0"
                                     "foo_bucket{le=\"0.1\",c=\"f\"} 0"
                                     "foo_bucket{le=\"0.25\",c=\"f\"} 0"
                                     "foo_bucket{le=\"0.5\",c=\"f\"} 0"
                                     "foo_bucket{le=\"1\",c=\"f\"} 0"
                                     "foo_bucket{le=\"2.5\",c=\"f\"} 0"
                                     "foo_bucket{le=\"5\",c=\"f\"} 0"
                                     "foo_bucket{le=\"10\",c=\"f\"} 0"
                                     "foo_bucket{le=\"+Inf\",c=\"f\"} 1"
                                     "foo_sum{c=\"d\"} 11"
                                     "foo_count{c=\"d\"} 2"
                                     "foo_bucket{le=\"0.005\",c=\"d\"} 0"
                                     "foo_bucket{le=\"0.01\",c=\"d\"} 0"
                                     "foo_bucket{le=\"0.025\",c=\"d\"} 0"
                                     "foo_bucket{le=\"0.05\",c=\"d\"} 0"
                                     "foo_bucket{le=\"0.1\",c=\"d\"} 0"
                                     "foo_bucket{le=\"0.25\",c=\"d\"} 0"
                                     "foo_bucket{le=\"0.5\",c=\"d\"} 0"
                                     "foo_bucket{le=\"1\",c=\"d\"} 1"
                                     "foo_bucket{le=\"2.5\",c=\"d\"} 1"
                                     "foo_bucket{le=\"5\",c=\"d\"} 1"
                                     "foo_bucket{le=\"10\",c=\"d\"} 2"
                                     "foo_bucket{le=\"+Inf\",c=\"d\"} 2"
                                     "")
                               "\n"))))

(struct gc-info
  (mode pre-amount pre-admin-amount code-amount
        post-amount post-admin-amount
        start-process-time end-process-time
        start-time end-time)
  #:prefab)

(define (prometheus-start-runtime-collector #:registry (registry (current-prometheus-registry))
                                            #:interval (interval 15000))
  (let ((worker (thread
                 (lambda ()
                   (let  ((gc-log (make-log-receiver (current-logger) 'debug 'GC))
                          (timer (lambda (t) (alarm-evt (+ (current-inexact-milliseconds) t))))
                          (gc-run (make-prometheus-metric-counter 'racket_runtime_gc_run))
                          (gc-time-metric (make-prometheus-metric-gauge 'racket_runtime_gc_time))
                          (gc-code-amount-metric (make-prometheus-metric-gauge 'racket_runtime_gc_code_amount))
                          (gc-post-amount-metric (make-prometheus-metric-gauge 'racket_runtime_gc_post_amount))
                          (gc-post-admin-amount-metric (make-prometheus-metric-gauge 'racket_runtime_gc_post_admin_amount))
                          (gc-process-time-metric (make-prometheus-metric-gauge 'racket_runtime_gc_process_time))
                          (gc-current-milliseconds-metric (make-prometheus-metric-gauge 'racket_runtime_gc_current_milliseconds))
                          (current-memory-use-metric (make-prometheus-metric-gauge 'racket_runtime_memory_use))
                          (current-process-milliseconds-metric (make-prometheus-metric-gauge 'racket_runtime_process_milliseconds)))
                     (for ((metric (list gc-run
                                         gc-code-amount-metric
                                         gc-post-amount-metric
                                         gc-post-admin-amount-metric
                                         gc-process-time-metric
                                         gc-time-metric
                                         gc-current-milliseconds-metric
                                         current-memory-use-metric
                                         current-process-milliseconds-metric)))
                       (prometheus-register! metric #:registry registry))
                     (let loop ((timer-evt (timer interval)))
                       (match (sync (thread-receive-evt) gc-log timer-evt)
                         ((vector level message (gc-info mode pre-amount pre-admin-amount code-amount
                                                         post-amount post-admin-amount
                                                         start-process-time end-process-time
                                                         start-time end-time) topic)
                          (let ((labels `((mode . ,mode))))
                            (prometheus-increment! gc-run
                                                   #:labels labels)
                            (prometheus-set! gc-code-amount-metric
                                             code-amount
                                             #:labels labels)
                            (prometheus-set! gc-post-amount-metric
                                             post-amount
                                             #:labels labels)
                            (prometheus-set! gc-post-admin-amount-metric
                                             post-admin-amount
                                             #:labels labels)
                            (prometheus-increment! gc-process-time-metric
                                                   #:by (- end-process-time start-process-time)
                                                   #:labels labels)
                            (prometheus-increment! gc-time-metric
                                                   #:by (- end-time start-time)
                                                   #:labels labels))
                          (loop timer-evt))
                         ((app (lambda (v) (eq? v timer-evt)) #t)
                          (prometheus-set! gc-current-milliseconds-metric (current-gc-milliseconds))
                          (prometheus-set! current-memory-use-metric (current-memory-use))
                          (prometheus-set! current-process-milliseconds-metric (current-process-milliseconds))
                          (loop (timer interval)))
                         ((app (lambda (v) (eq? v (thread-receive-evt))) #t)
                          (void (thread-receive))))))))))
    (thunk (thread-send worker 'exit))))

(define ((make-prometheus-http-handler #:registry (registry (current-prometheus-registry))) _)
  (response/full
   200 #"OK"
   (current-seconds)
   #"text/plain"
   null
   (list (string->bytes/utf-8 (prometheus-metrics-format #:registry registry)))))
