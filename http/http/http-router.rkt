#lang racket
(require web-server/http
         net/url
         (for-syntax corpix/syntax))
(provide (all-defined-out))

(define current-routes (make-parameter (make-hash)))
(define current-routes-index (make-parameter (make-hash)))
(define current-route-path-separator (make-parameter "/"))
(define current-route-define-mode (make-parameter 'loose)) ;; or 'strict
(define current-route-matchers (make-parameter (make-hash)))

(define-struct route (name method path handler) #:transparent)
(define-struct route-matcher (name priority match apply) #:transparent)
(define-struct route-tree-node (value (route #:mutable) matcher (childs #:mutable))
  #:constructor-name -make-route-tree-node
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
                     (-make-route-tree-node input route matcher null))))))
        (error (format "no matcher for input ~v" input)))))

(define (make-route-tree route)
  (let* ((separator (current-route-path-separator))
         (path (route-path route))
         (root-node (for/fold ((acc #f))
                              ((input (reverse (string-split path separator))))
                      (let ((route-tree-node (make-route-tree-node input (if acc #f route))))
                        (begin0 route-tree-node
                          (when acc
                            (set-route-tree-node-childs! route-tree-node (list acc))))))))
    (if root-node (list root-node) null)))

(define (merge-route-tree route-trees
                          #:key (key (lambda (node)
                                       (cons (route-matcher-name (route-tree-node-matcher node))
                                             (route-tree-node-value node)))))
  (for/fold ((acc null))
            ((group (in-list (group-by key route-trees))))
    (let ((route-tree-node-acc (struct-copy route-tree-node (car group))))
      (begin0 (cons route-tree-node-acc acc)
        (for ((route-tree-node (in-list (cdr group))))
          (let ((childs (merge-route-tree
                         (append (route-tree-node-childs route-tree-node-acc)
                                 (route-tree-node-childs route-tree-node))
                         #:key key)))
            (set-route-tree-node-childs! route-tree-node-acc childs)
            (let ((route (route-tree-node-route route-tree-node)))
              (when route
                (set-route-tree-node-route! route-tree-node-acc route)))))))))

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
    (hash-set! index method (merge-route-tree
                             (append (hash-ref index method null)
                                     (make-route-tree route))))))

(define-syntax (define-route stx)
  (syntax-parse stx
    ((_ (name argument ...)
        #:method method
        #:path path
        body ...)
     (syntax/loc stx
       (let* ((sym-name 'name)
              (sym-method (string->bytes/utf-8 (string-upcase (symbol->string 'method))))
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

(define (dispatch-route-tree tree inputs)
  (let loop ((tree tree)
             (inputs inputs))
    (and (pair? inputs)
         (let ((input (car inputs)))
           (for/fold ((acc #f))
                     ((node (in-list tree)))
             (let ((apply (route-matcher-apply (route-tree-node-matcher node))))
               (and (apply node input)
                    (if (pair? (cdr inputs))
                        (loop (route-tree-node-childs node) (cdr inputs))
                        (and (route-tree-node-route node) node)))))))))

(define (dispatch-route method path)
  (let* ((separator (current-route-path-separator))
         (index (current-routes-index))
         (index-bucket (hash-ref index method #f))
         (route-tree-node (and index-bucket (dispatch-route-tree
                                             index-bucket
                                             (string-split path separator)))))
    (and route-tree-node
         (route-tree-node-route route-tree-node))))

(define-route-matcher static
  #:priority 1
  (match string?)
  (apply (lambda (node input) (equal? (route-tree-node-value node) input))))

(define-route-matcher wildcard
  #:priority 2
  (match (let ((rx (regexp ".*[\\*]+.*")))
           (lambda (input) (and (string? input)
                                (regexp-match rx input)))))
  (apply (lambda (node input) #f)))

(define-route-matcher parameter
  #:priority 3
  (match (let ((rx (regexp ":[a-zA-Z][a-zA-Z0-9]+")))
           (lambda (input) (and (string? input)
                                (regexp-match rx input)))))
  (apply (lambda (node input) #f)))

(define (http-dispatch-route request)
  (let ((route (dispatch-route (request-method request)
                               (url->string (request-uri request)))))
    (if route
        ((route-handler route) request)
        (response/output (lambda (out) (displayln "not found" out))
                         #:code 404))))
