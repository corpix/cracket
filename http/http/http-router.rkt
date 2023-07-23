#lang racket
(require web-server/servlet
         (for-syntax corpix/syntax))

(define current-routes (make-parameter (make-hash)))
(define current-route-segment-separator (make-parameter "/"))
(define current-route-define-mode (make-parameter 'loose)) ;; or 'strict

(define-struct route (method path handler) #:transparent)

(define-syntax (define-route stx)
  (syntax-parse stx
    ((_ (name argument ...)
        #:method method
        #:path path
        body ...)
     (syntax/loc (syntax (body ...))
       (let* ((route-name 'name)
              (method-name 'method)
              (route (hash-ref (current-routes) route-name #f)))
         (when (and (eq? (current-route-define-mode) 'strict) route)
           (error (format "route with name ~v already defined" route-name)))
         (hash-set! (current-routes) route-name
                    (make-route method-name path (lambda (argument ...) body ...))))))))

(define-route (name request response)
  #:method get
  #:path "/name/:parameter1/:parameter2"
  (displayln "handling route"))

;;

(define-struct route-segment-handler (name priority match dispatch) #:transparent)
(define-struct route-segment (name input (childs #:mutable)) #:transparent)

(define current-route-segment-handlers (make-parameter (make-hash)))

(define (route-segment-handler-set! table name priority handler)
  (for ((priority (in-list (hash-keys table))))
    (let* ((priority-table (hash-ref table priority))
           (pred (hash-ref priority-table name #f)))
      (when pred
        (when (eq? (current-route-define-mode) 'strict)
          (error (format "segment with name ~v already defined" name)))
        ;; NOTE: we may improve... because only one instance could exists
        ;; no need to loop on the tail of the key-set
        (hash-remove! priority-table name))))
  (let ((priority-table (or (hash-ref table priority #f)
                            (make-hash))))
    (hash-set! priority-table name handler)
    (hash-set! table priority priority-table)))

(define (route-segment-handle input)
  (let ((segments-table (current-route-segment-handlers))
        (result #f))
    (for ((priority (in-list (sort (hash-keys segments-table) >))))
      #:break result
      (let* ((priority-table (hash-ref segments-table priority)))
        (for ((segment-name (sort (hash-keys priority-table) symbol<?)))
          #:break result
          (let* ((handler (hash-ref priority-table segment-name))
                 (handler-match (route-segment-handler-match handler)))
            (when (handler-match input)
              (set! result (make-route-segment segment-name input null)))))))
    (or result
        (error (format "no handler for route segment ~v" input)))))

(define-syntax (define-route-segment-handler stx)
  (syntax-parse stx
    #:datum-literals (match dispatch)
    ((_ name (~optional (~seq #:priority prio) #:defaults ((prio #'1)))
        (match match-proc)
        (dispatch dispatch-proc))
     (syntax (let ((handler-name 'name)
                   (handler-priority prio))
               (route-segment-handler-set! (current-route-segment-handlers) handler-name handler-priority
                                           (make-route-segment-handler handler-name handler-priority
                                                                       match-proc dispatch-proc)))))))

(define-route-segment-handler static
  #:priority 1
  (match string?)
  (dispatch (lambda (segment) (void))))

(define-route-segment-handler wildcard
  #:priority 2
  (match (let ((rx (regexp ".*[\\*]+.*")))
           (lambda (input) (and (string? input)
                                (regexp-match rx input)))))
  (dispatch (lambda (segment) (void))))

(define-route-segment-handler parameter
  #:priority 3
  (match (let ((rx (regexp ":[a-zA-Z][a-zA-Z0-9]+")))
           (lambda (input) (and (string? input)
                                (regexp-match rx input)))))
  (dispatch (lambda (segment) (void))))

(define (route->segments route)
  (for/fold ((acc #f))
            ((input (reverse (string-split route (current-route-segment-separator)))))
    (let ((segment (route-segment-handle input)))
      (begin0 segment
        (when acc
          (set-route-segment-childs! segment (list acc)))))))

(define (route-segments-merge segments
                              #:key (key (lambda (segment)
                                           (cons (route-segment-name segment)
                                                 (route-segment-input segment)))))
  (for/fold ((acc null))
            ((group (in-list (group-by key segments))))
    (let ((segment (car group)))
      (begin0 (cons segment acc)
        (for ((subject (in-list (cdr group))))
          (set-route-segment-childs! segment
                                     (route-segments-merge
                                      (append (route-segment-childs segment)
                                              (route-segment-childs subject))
                                      #:key key)))))))

;;(route-segment-handle 'foo)
(route-segments-merge (list (route->segments "/foo/bar")
                            (route->segments "/foo/baz")
                            (route->segments "/foo/bar/qux")
                            (route->segments "/foo/bar/bux")))
