#lang racket
(require web-server/servlet
         (for-syntax corpix/syntax))

(define current-routes (make-parameter (make-hash)))
(define current-routes-index (make-parameter (make-hash)))
(define current-route-path-separator (make-parameter "/"))
(define current-route-define-mode (make-parameter 'loose)) ;; or 'strict
(define current-route-matchers (make-parameter (make-hash)))

(define-struct route (name method path handler) #:transparent)
(define-struct route-matcher (name priority match apply) #:transparent)
(define-struct route-tree (value route matcher (childs #:mutable))
  #:constructor-name -make-route-tree
  #:transparent)

(define (make-route-tree-node input (route #f))
  (let ((matchers (current-route-matchers)))
    (or (for/fold ((result #f))
                  ((priority (in-list (sort (hash-keys matchers) >))))
          #:break result
          (let* ((matchers (hash-ref matchers priority)))
            (for/fold ((result result))
                      ((matcher-name (sort (hash-keys matchers) symbol<?)))
              #:break result
              (let* ((matcher (hash-ref matchers matcher-name))
                     (match-proc (route-matcher-match matcher)))
                (and (match-proc input)
                     (-make-route-tree input route matcher null))))))
        (error (format "no matcher for input ~v" input)))))

(define (make-route-tree route)
  (let ((separator (current-route-path-separator))
        (path (route-path route)))
    (for/fold ((acc #f))
              ((input (reverse (string-split path separator))))
      (let ((route-tree (make-route-tree-node input
                                              ;; route should appear only on leafs
                                              (if acc #f route))))
        (begin0 route-tree
          (when acc
            (set-route-tree-childs! route-tree (list acc))))))))

(define (route-tree-merge route-trees
                          #:key (key (lambda (node)
                                       (cons (route-matcher-name (route-tree-matcher node))
                                             (route-tree-value node)))))
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


(define (set-route-matcher! name priority handler)
  (let ((matchers (current-route-matchers)))
    (for ((priority (in-list (hash-keys matchers))))
      (let* ((matchers-bucket (hash-ref matchers priority))
             (pred (hash-ref matchers-bucket name #f)))
        (when pred
          (when (eq? (current-route-define-mode) 'strict)
            (error (format "segment with name ~v already defined" name)))
          (hash-remove! matchers-bucket name))))
    (let ((matchers-bucket (or (hash-ref matchers priority #f)
                               (make-hash))))
      (hash-set! matchers-bucket name handler)
      (hash-set! matchers priority matchers-bucket))))

(define (find-route name)
  (let ((routes (current-routes)))
    (for/fold ((route #f))
              ((method (hash-keys routes)))
      #:break route
      (let* ((routes-bucket (hash-ref routes method)))
        (hash-ref routes-bucket name #f)))))

(define (set-route! method route)
  (let* ((routes (current-routes))
         (routes-bucket (or (hash-ref routes method #f)
                            (make-hash)))
         (index (current-routes-index)))
    (hash-set! routes-bucket (route-name route) route)
    (hash-set! routes method routes-bucket)
    (hash-set! index method (route-tree-merge
                             (append (hash-ref index method null)
                                     (list (make-route-tree route)))))))

(define-syntax (define-route stx)
  (syntax-parse stx
    ((_ (name argument ...)
        #:method method
        #:path path
        body ...)
     (syntax/loc stx
       (let* ((sym-name 'name)
              (sym-method 'method)
              (sym-path path))
         (when (eq? (current-route-define-mode) 'strict)
           (let ((route (find-route sym-name)))
             (when route
               (error (format "route with name ~v already defined for method ~v with path ~v"
                              sym-name (route-method route) (route-path route))))))
         (set-route! sym-method (make-route sym-name sym-method sym-path
                                            (lambda (argument ...) body ...))))))))

(define-syntax (define-route-matcher stx)
  (syntax-parse stx
    #:datum-literals (match apply)
    ((_ name
        (~optional (~seq #:priority prio) #:defaults ((prio #'1)))
        (match match-proc)
        (apply apply-proc))
     (syntax (let ((handler-name 'name)
                   (handler-priority prio))
               (set-route-matcher! handler-name handler-priority
                                   (make-route-matcher handler-name handler-priority
                                                       match-proc apply-proc)))))))

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

(define-route (name2 request response)
  #:method get
  #:path "/name2/:parameter1/:parameter2"
  (displayln "handling route"))

(current-routes)
(current-routes-index)
