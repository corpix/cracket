#lang racket
(require (for-syntax racket))
(provide (except-out (struct-out prometheus-registry) -make-prometheus-registry)
         make-prometheus-registry
         current-prometheus-registry
         prometheus-register!
         prometheus-ref)
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
  (-make-prometheus-registry (make-hasheq) namespace labels
                             (make-semaphore 1)))

(define current-prometheus-registry
  (make-parameter (make-prometheus-registry)))

;; (define current-prometheus-histogram-buckets
;;   (make-parameter (list 0.005 0.01
;;                         0.025 0.05
;;                         0.1 0.25
;;                         0.5 1
;;                         2.5 5
;;                         10 +inf.0)))

;;

(define-struct prometheus-metric
  (type name store doc labels semaphore)
  #:transparent
  #:constructor-name -make-prometheus-metric)

(define (make-prometheus-metric type name
                                #:store (store (make-hash))
                                #:doc (doc #f)
                                #:labels (labels null))
  (-make-prometheus-metric type name store doc labels
                           (make-semaphore 1)))
;;

(define (prometheus-register-metric! name metric #:registry (registry (current-prometheus-registry)))
  (call-with-semaphore
   (prometheus-registry-semaphore registry)
   (thunk (let ((metrics (prometheus-registry-metrics registry)))
            (when (hash-ref metrics name #f)
              (error (format "metric ~s already exists" name)))
            (hash-set! metrics name metric)))))

(define -prometheus-ref-default (gensym))
(define (prometheus-ref name (default -prometheus-ref-default)
                        #:registry (registry (current-prometheus-registry)))
  (let ((value (call-with-semaphore
                (prometheus-registry-semaphore registry)
                (thunk (hash-ref (prometheus-registry-metrics registry) name default)))))
    (when (eq? value -prometheus-ref-default)
      (error (format "no metric named ~s in registry" name)))
    value))

(define (prometheus-register! type name
                              #:registry (registry (current-prometheus-registry))
                              #:store (store (make-hash))
                              #:doc (doc #f)
                              #:labels (labels null))
  (prometheus-register-metric!
   name
   (make-prometheus-metric type name
                           #:store store
                           #:doc doc
                           #:labels labels)
   #:registry registry))

(define (prometheus-unregister! name #:registry (registry (current-prometheus-registry)))
  (call-with-semaphore
   (prometheus-registry-semaphore registry)
   (thunk (let ((metrics (prometheus-registry-metrics registry)))
            (unless (hash-ref metrics name #f)
              (error (format "metric ~s does not exists" name)))
            (hash-remove! metrics name)))))

(define (prometheus-increment-metric! metric
                                      #:by (by 1)
                                      #:labels (labels null))
  (let ((metric-types '(counter gauge)))
    (unless (memq (prometheus-metric-type metric) metric-types)
      (error (format "only works with metric types: ~a" metric-types))))
  (let ((store (prometheus-metric-store metric)))
    (call-with-semaphore
     (prometheus-metric-semaphore metric)
     (thunk (hash-set! store labels (+ by (hash-ref store labels 0)))))))

(define (prometheus-increment! name
                               #:registry (registry (current-prometheus-registry))
                               #:by (by 1)
                               #:labels (labels null))
  (prometheus-increment-metric! (prometheus-ref name #:registry registry)
                                #:by by
                                #:labels labels))

(module+ test
  (test-case "make-prometheus-registry"
    (let ((registry (make-prometheus-registry)))
      (check-equal? (prometheus-registry-metrics registry)
                    (make-hasheq))
      (check-equal? (prometheus-registry-namespace registry)
                    #f)
      (check-equal? (prometheus-registry-labels registry)
                    null))
    (let ((registry (make-prometheus-registry
                     #:namespace "test"
                     #:labels '((a . b) (c . d)))))
      (check-equal? (prometheus-registry-metrics registry)
                    (make-hasheq))
      (check-equal? (prometheus-registry-namespace registry)
                    "test")
      (check-equal? (prometheus-registry-labels registry)
                    '((a . b) (c . d)))))
  (test-case "prometheus-register-metric!"
    (parameterize ((current-prometheus-registry (make-prometheus-registry)))
      (check-equal? (prometheus-registry-metrics (current-prometheus-registry))
                    (make-hasheq))
      (let ((metric (make-prometheus-metric 'counter 'foo)))
        (prometheus-register-metric! 'foo metric)
        (check-equal? (prometheus-registry-metrics (current-prometheus-registry))
                      (make-hasheq `((foo . ,metric)))))))
(test-case "prometheus-ref"
    (parameterize ((current-prometheus-registry (make-prometheus-registry)))
      (let ((metric (make-prometheus-metric 'counter 'foo)))
        (prometheus-register-metric! 'foo metric)
        (check-equal? (prometheus-ref 'foo) metric))))
  (test-case "prometheus-register!"
    (parameterize ((current-prometheus-registry (make-prometheus-registry)))
      (check-equal? (prometheus-registry-metrics (current-prometheus-registry))
                    (make-hasheq))
      (prometheus-register! 'counter 'foo)
      (check-equal? (prometheus-metric-type (prometheus-ref 'foo #:registry (current-prometheus-registry)))
                    'counter)))
  (test-case "prometheus-unregister!"
    (parameterize ((current-prometheus-registry (make-prometheus-registry)))
      (prometheus-register-metric! 'foo (make-prometheus-metric 'counter 'foo))
      (prometheus-unregister! 'foo)
      (check-equal? (prometheus-registry-metrics (current-prometheus-registry))
                    (make-hasheq))))
  (test-case "make-prometheus-metric"
    (let ((metric (make-prometheus-metric 'foo 'bar)))
      (check-equal? (prometheus-metric-type metric)
                    'foo)
      (check-equal? (prometheus-metric-name metric)
                    'bar)
      (check-equal? (prometheus-metric-store metric)
                    (make-hash))
      (check-equal? (prometheus-metric-doc metric)
                    #f)
      (check-equal? (prometheus-metric-labels metric)
                    null))))

;;

(define format-prometheus-label
  (match-lambda
    ((cons label value)
     (format "~a=\"~a\"" label value))))

(define (format-prometheus-labels labels)
  (if (null? labels) ""
      (string-append "{" (string-join (map format-prometheus-label labels) ",") "}")))

(define (format-prometheus-metric name labels value)
  (format "~a~a ~a\n" name (format-prometheus-labels labels) value))

(define (write-prometheus-metric name labels value (port (current-output-port)))
  (write-string (format-prometheus-metric name labels value) port))

(define (write-metrics (port (current-output-port)) #:registry (registry (current-prometheus-registry)))
  (for (((name metric) (in-hash (prometheus-registry-metrics registry))))
    (let* ((namespace (prometheus-registry-namespace registry))
           (name (if namespace (string-append namespace "_" name) name))
           (labels (prometheus-metric-labels metric))
           (type (prometheus-metric-type metric))
           (doc (prometheus-metric-doc metric))
           (help-string (and doc (format "# HELP ~a ~a\n" name doc)))
           (type-string (format "# TYPE ~a ~a\n" name type)))
      (when help-string (write-string help-string port))
      (write-string type-string port)
      (for (((metric-labels value) (in-hash (prometheus-metric-store metric))))
        (write-prometheus-metric name
                                 (append labels metric-labels)
                                 value port)))))

(module+ test
  (test-case "format-prometheus-label"
    (check-equal? (format-prometheus-label (cons "test" 1))
                  "test=\"1\"")
    (check-equal? (format-prometheus-label (cons 'test 1))
                  "test=\"1\"")
    (check-equal? (format-prometheus-label (cons 'test 1.2))
                  "test=\"1.2\"")
    (check-equal? (format-prometheus-label (cons 'test "hello"))
                  "test=\"hello\"")
    (check-equal? (format-prometheus-label (cons 'test 'hello))
                  "test=\"hello\""))
  (test-case "format-prometheus-labels"
    (check-equal? (format-prometheus-labels '((1 . 2) (3 . 4)))
                  "{1=\"2\",3=\"4\"}"))
  (test-case "write-metrics"
    (check-equal? (call-with-output-string
                   (lambda (out)
                     (parameterize ((current-output-port out)
                                    (current-prometheus-registry (make-prometheus-registry)))
                       (prometheus-register! 'counter 'foo #:doc "test")
                       (prometheus-increment! 'foo #:labels '((c . d)))
                       (prometheus-increment! 'foo #:labels '((c . d)))
                       (prometheus-increment! 'foo #:labels '((c . f)))
                       (prometheus-register! 'counter 'bar #:labels '((a . b)))
                       (prometheus-increment! 'bar #:labels '((c . d)))
                       (write-metrics))))
                  (string-join (list "# TYPE bar counter"
                                     "bar{a=\"b\",c=\"d\"} 1"
                                     "# HELP foo test"
                                     "# TYPE foo counter"
                                     "foo{c=\"f\"} 1"
                                     "foo{c=\"d\"} 2"
                                     "")
                               "\n"))))
