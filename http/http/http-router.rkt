#lang racket
(require web-server/servlet
         (for-syntax corpix/syntax))

(define current-routes (make-parameter (make-hash)))
(define current-route-path-separator (make-parameter "/"))
(define current-route-define-mode (make-parameter 'loose)) ;; or 'strict
(define current-route-matchers (make-parameter (make-hash)))

(define-struct route (name method path path-tree handler) #:transparent)
(define-struct route-matcher (name priority match apply) #:transparent)
(define-struct route-tree (value matcher (childs #:mutable)) #:transparent)

(define (route-segment-parse input)
  (or (let ((matchers-by-priority (current-route-matchers)))
        (for/fold ((result #f))
                  ((priority (in-list (sort (hash-keys matchers-by-priority) >))))
          #:break result
          (let* ((matchers (hash-ref matchers-by-priority priority)))
            (for/fold ((result result))
                      ((matcher-name (sort (hash-keys matchers) symbol<?)))
              #:break result
              (let* ((matcher (hash-ref matchers matcher-name))
                     (match-proc (route-matcher-match matcher)))
                (and (match-proc input)
                     (make-route-tree input matcher null)))))))
      (error (format "no matcher for input ~v" input))))

(define (route-path-parse path)
  (for/fold ((acc #f))
            ((input (reverse (string-split path (current-route-path-separator)))))
    (let ((route-tree (route-segment-parse input)))
      (begin0 route-tree
        (when acc
          (set-route-tree-childs! route-tree (list acc)))))))

(define (route-tree-merge route-trees
                          #:key (key (lambda (segment)
                                       (cons (route-matcher-name segment)
                                             (route-tree-value segment)))))
  (for/fold ((acc null))
            ((group (in-list (group-by key route-trees))))
    (let ((route-tree (car group)))
      (begin0 (cons route-tree acc)
        (for ((subject (in-list (cdr group))))
          (let ((childs (route-tree-merge
                         (append (route-tree-childs route-tree)
                                 (route-tree-childs subject))
                         #:key key)))
            (set-route-tree-childs! route-tree childs)))))))


(define (set-route-matcher! name priority handler #:table (table (current-route-matchers)))
  (for ((priority (in-list (hash-keys table))))
    (let* ((matchers (hash-ref table priority))
           (pred (hash-ref matchers name #f)))
      (when pred
        (when (eq? (current-route-define-mode) 'strict)
          (error (format "segment with name ~v already defined" name)))
        (hash-remove! matchers name))))
  (let ((matchers (or (hash-ref table priority #f)
                      (make-hash))))
    (hash-set! matchers name handler)
    (hash-set! table priority matchers)))

(define (find-route name #:table (table (current-routes)))
  (for/fold ((route #f))
            ((method (hash-keys table)))
    #:break route
    (let* ((method-table (hash-ref table method)))
      (hash-ref method-table name #f))))

(define (set-route! method route #:table (table (current-routes)))
  (let ((method-routes (hash-ref table method (make-hash))))
    (hash-set! method-routes (route-name route) route)
    (hash-set! table method method-routes)))

(define-syntax (define-route stx)
  (syntax-parse stx
    ((_ (name argument ...)
        #:method method
        #:path path
        (~optional (~seq #:table table) #:defaults ((table #'(current-routes))))
        body ...)
     (syntax/loc stx
       (let* ((sym-name 'name)
              (sym-method 'method)
              (sym-path path)
              (sym-table table))
         (when (eq? (current-route-define-mode) 'strict)
           (let ((route (find-route sym-name #:table sym-table)))
             (when route
               (error (format "route with name ~v already defined for method ~v with path ~v"
                              sym-name (route-method route) (route-path route))))))
         (set-route! sym-method (make-route sym-name sym-method
                                            sym-path (route-path-parse sym-path)
                                            (lambda (argument ...) body ...))
                     #:table sym-table))))))

(define-syntax (define-route-matcher stx)
  (syntax-parse stx
    #:datum-literals (match apply)
    ((_ name
        (~optional (~seq #:priority prio) #:defaults ((prio #'1)))
        (~optional (~seq #:table table) #:defaults ((table #'(current-route-matchers))))
        (match match-proc)
        (apply apply-proc))
     (syntax (let ((handler-name 'name)
                   (handler-priority prio)
                   (matchers-table table))
               (set-route-matcher! handler-name handler-priority
                                   (make-route-matcher handler-name handler-priority
                                                       match-proc apply-proc)
                                   #:table matchers-table))))))

(define-route-matcher static
  #:priority 1
  (match string?)
  (apply (lambda (segment input) (equal? (route-tree-value segment) input))))

(define-route-matcher wildcard
  #:priority 2
  (match (let ((rx (regexp ".*[\\*]+.*")))
           (lambda (input) (and (string? input)
                                (regexp-match rx input)))))
  (apply (lambda (segment input) #f)))

(define-route-matcher parameter
  #:priority 3
  (match (let ((rx (regexp ":[a-zA-Z][a-zA-Z0-9]+")))
           (lambda (input) (and (string? input)
                                (regexp-match rx input)))))
  (apply (lambda (segment input) #f)))

;; (route-path-segments-merge (list (route->segments "/foo/bar")
;;                             (route->segments "/foo/baz")
;;                             (route->segments "/foo/bar/qux*")
;;                             (route->segments "/foo/bar/bux/:param")))

;; (define (xxx segments inputs)
;;   (let loop ((segments segments)
;;              (inputs inputs))
;;     (and (pair? inputs)
;;          (let ((input (car inputs)))
;;            (for/fold ((acc #f))
;;                      ((segment (in-list segments)))
;;              (let ((apply (route-path-segment-apply segment)))
;;                (and (apply segment input)
;;                     (if (not (pair? (cdr inputs)))
;;                         (not (pair? (route-tree-childs segment)))
;;                         ;; hmmm, I need a handler here^
;;                         ;; but we are working with segment, not a route
;;                         ;; how do we connect segment and route concepts?
;;                         (loop (route-tree-childs segment) (cdr inputs))))))))))

;; (displayln (xxx (route-path-segments-merge (list (route->segments "/foo/bar")
;;                                  (route->segments "/foo/baz")
;;                                  (route->segments "/foo/bar/qux*")
;;                                  (route->segments "/foo/bar/bux/:param")))
;;      (string-split "/foo" (current-route-path-separator))))

(define-route (name request response)
  #:method get
  #:path "/name/:parameter1/:parameter2"
  (displayln "handling route"))

(define-route (name request response)
  #:method get
  #:path "/name/:parameter1/:parameter2"
  (displayln "handling route"))

(current-route-matchers)
(current-routes)
