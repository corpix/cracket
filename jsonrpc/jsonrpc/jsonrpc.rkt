#lang racket
(require racket/async-channel
         racket/tcp
         corpix/json
         (for-syntax corpix/syntax))
(module+ test
  (require rackunit))

(define-syntax (make-jsonrpc-id-counter stx)
  (syntax-parse stx
    ((_ (~optional start #:defaults ((start #'0))))
     (syntax (let* ((counter start)
                    (sem (make-semaphore 1))
                    (next (thunk (begin0 counter (set! counter (+ 1 counter))))))
               (thunk (call-with-semaphore sem next)))))))

(define-struct jsonrpc-method
  (name signature doc proc)
  #:transparent)

(define-struct jsonrpc-request-registry
  (channels sem)
  #:constructor-name -make-jsonrpc-request-registry
  #:transparent)

(define-struct jsonrpc-client (in out closer)
  #:transparent)

(define (close-jsonrpc-client client)
  ((jsonrpc-client-closer client)))

(define (make-jsonrpc-request-registry)
  (-make-jsonrpc-request-registry (make-hash)
                                  (make-semaphore 1)))

;;

(define jsonrpc-version "2.0")
(define current-jsonrpc-input-port
  (make-derived-parameter (make-parameter #f)
                          values
                          (lambda (port)
                            (begin0 port
                              (unless port (error "no input jsonrpc port is active"))))))

(define current-jsonrpc-output-port
  (make-derived-parameter (make-parameter #f)
                          values
                          (lambda (port)
                            (begin0 port
                              (unless port (error "no output jsonrpc port is active"))))))

(define current-jsonrpc-id-generator (make-parameter (make-jsonrpc-id-counter)))

(define current-jsonrpc-method-registry (make-parameter (make-hash)))
(define current-jsonrpc-request-registry (make-parameter (make-jsonrpc-request-registry)))

;;

(define (write-jsonrpc-message id payload (port (current-jsonrpc-output-port)))
  (write-json (make-hasheq `((jsonrpc . ,jsonrpc-version) (id . ,id) ,@payload)) port)
  (flush-output port))

(define (write-jsonrpc-request method (port (current-jsonrpc-output-port))
                               #:params (params #f)
                               #:next-id (next-id (current-jsonrpc-id-generator)))
  (let ((id (next-id)))
    (begin0 id
      (let* ((payload `((method . ,method)))
             (payload (if params (cons `(params . ,params) payload) payload)))
        (write-jsonrpc-message id payload port)))))

(define (write-jsonrpc-response id (port (current-jsonrpc-output-port))
                                #:result (result #f)
                                #:error (error #f))
  (let* ((payload (if error
                      `((error . ,error))
                      `((result . ,(if (void? result) 'null result))))))
    (write-jsonrpc-message id payload port)))

(define (read-jsonrpc-message (port (current-jsonrpc-input-port)))
  (let* ((message (read-json port))
         (message-version (hash-ref message "jsonrpc" #f)))
    (begin0 message
      (unless message-version
        (error (format "jsonrpc version is not specified in incoming message: ~a" message)))
      (unless (equal? message-version jsonrpc-version)
        (error (format "jsonrpc version mismatch: expected ~s, got ~s" jsonrpc-version message-version))))))

(define (read-jsonrpc-request (port (current-jsonrpc-input-port)))
  (let* ((message (read-jsonrpc-message port)))
    (begin0 message
      (unless (hash-has-key? message "method")
        (error (format "jsonrpc method is not specified: ~a" message))))))

(define (read-jsonrpc-response (port (current-jsonrpc-input-port)))
  (let* ((message (read-jsonrpc-message port)))
    (begin0 message
      (unless (or (hash-has-key? message "result")
                  (hash-has-key? message "error"))
        (error (format "jsonrpc message has not result|error field: ~a" message))))))

(module+ test
  (test-case "write-jsonrpc-request"
    (check-equal?
     (let ((out (open-output-string)))
       (parameterize ((current-jsonrpc-id-generator (make-jsonrpc-id-counter 777)))
         (write-jsonrpc-request 'foo out #:params '(1 2 3)))
       (read-jsonrpc-request (open-input-string (get-output-string out))))
     (make-immutable-hash '(("jsonrpc" . "2.0")
                            ("id" . 777)
                            ("method" . "foo")
                            ("params" . (1 2 3)))))
    (check-equal?
     (let ((out (open-output-string)))
       (parameterize ((current-jsonrpc-id-generator (make-jsonrpc-id-counter 777)))
         (write-jsonrpc-request 'foo out #:params '(1 2 3))
         (write-jsonrpc-request 'foo out #:params '(4 5 6))
         (write-jsonrpc-request 'foo out #:params '(7 8 9)))
       (let ((port (open-input-string (get-output-string out))))
         (list (read-jsonrpc-request port)
               (read-jsonrpc-request port)
               (read-jsonrpc-request port))))
     (list (make-immutable-hash '(("jsonrpc" . "2.0")
                                  ("id" . 777)
                                  ("method" . "foo")
                                  ("params" . (1 2 3))))
           (make-immutable-hash '(("jsonrpc" . "2.0")
                                  ("id" . 778)
                                  ("method" . "foo")
                                  ("params" . (4 5 6))))
           (make-immutable-hash '(("jsonrpc" . "2.0")
                                  ("id" . 779)
                                  ("method" . "foo")
                                  ("params" . (7 8 9)))))))
  (test-case "write-jsonrpc-response"
    (check-equal?
     (let ((out (open-output-string)))
       (write-jsonrpc-response 666 out #:result '(a b c))
       (read-jsonrpc-response (open-input-string (get-output-string out))))
     (make-immutable-hash '(("jsonrpc" . "2.0")
                            ("id" . 666)
                            ("result" . ("a" "b" "c")))))))

;;

(define-syntax (define-jsonrpc stx)
  (syntax-parse stx
    ((_ (name rest ...) (~optional body0 #:defaults ((body0 #'#f))) bodyN ...)
     (let ((doc-datum (syntax->datum #'body0)))
       (with-syntax* ((name-string (symbol->string (syntax->datum #'name)))
                      (signature (let loop ((inner-stx #'(rest ...))
                                            (acc null))
                                   (if (pair? (syntax->datum inner-stx))
                                       (syntax-parse inner-stx
                                         (((~seq key:keyword _) rest ...)
                                          (loop #'(rest ...) (append acc (list #`(keyword #,(string->symbol (keyword->string (syntax->datum #'key))))))))
                                         (((key:id _) rest ...)
                                          (loop #'(rest ...) (append acc (list #'(symbol key)))))
                                         ((key:id rest ...)
                                          (loop #'(rest ...) (append acc (list #'(symbol key))))))
                                       acc)))
                      (params (let* ((key-id (lambda (stx) (syntax-parse stx
                                                             ((key _ ...) #'key)
                                                             (key #'key))))
                                     (extract-params (lambda (stx) (map key-id (syntax->list stx)))))
                                (syntax-parse #'(rest ...)
                                  (((~seq _:keyword value) ...)
                                   #`(make-hasheq (list #,@(map (lambda (param) `(cons ',param ,param))
                                                                (extract-params #'(value ...))))))
                                  ((value ...) #`(list #,@(extract-params #'(value ...)))))))
                      (doc (if (string? doc-datum)
                               (datum->syntax stx doc-datum)
                               #'#f))
                      ((body ...) (cond ((not (pair? (syntax->list #'(bodyN ...)))) #'((call-jsonrpc 'name #:params params)))
                                        ((string? doc-datum) #'(bodyN ...))
                                        (else #'(body0 bodyN ...)))))
         (syntax (begin
                   (define (name rest ...) body ...)
                   (hash-set! (current-jsonrpc-method-registry) name-string
                              (make-jsonrpc-method 'name 'signature doc name)))))))))

(define (jsonrpc-apply method params)
  (let ((proc (jsonrpc-method-proc method)))
    (if (hash? params)
        (keyword-apply proc
                       (map (lambda (key)
                              (string->keyword (if (symbol? key)
                                                   (symbol->string key)
                                                   key)))
                            (hash-keys params))
                       (hash-values params) null)
        (apply proc params))))

(define (dispatch-jsonrpc-request message #:method-registry (methods (current-jsonrpc-method-registry)))
  (let* ((method (hash-ref message "method"))
         (method-name (if (symbol? method) (symbol->string method) method))
         (method-descriptor (hash-ref methods method-name #f))
         (params (hash-ref message "params" null)))
    (unless method-descriptor
      (error (format "method ~s is not defined" method-name)))
    (jsonrpc-apply method-descriptor params)))

(define (dispatch-jsonrpc-response message
                                   #:request-registry (registry (current-jsonrpc-request-registry)))
  (let* ((id (hash-ref message "id"))
         (chan (call-with-semaphore
                (jsonrpc-request-registry-sem registry)
                (thunk (hash-ref (jsonrpc-request-registry-channels registry) id #f)))))
    (unless chan
      (error (format "jsonrpc response dispatch failed, no response channel found for id ~a" id)))
    (async-channel-put
     chan
     (or (hash-ref message "result" #f)
         (make-exn:fail (hash-ref message "error")
                        (current-continuation-marks))))))

(define (invoke-jsonrpc method (port (current-jsonrpc-output-port))
                        #:params (params null)
                        #:request-registry (registry (current-jsonrpc-request-registry)))
  (let ((id (write-jsonrpc-request method port #:params params))
        (chan (make-async-channel)))
    (begin0 chan
      (call-with-semaphore
       (jsonrpc-request-registry-sem registry)
       (thunk (hash-set! (jsonrpc-request-registry-channels registry)
                         id chan))))))

(module+ test
  (test-case "invoke-jsonrpc"
    (let ((out (open-output-string)))
      (parameterize ((current-jsonrpc-id-generator (make-jsonrpc-id-counter 777))
                     (current-jsonrpc-request-registry (make-jsonrpc-request-registry)))
        (invoke-jsonrpc 'foo out #:params '(1 2)))
      (check-equal? (read-json (open-input-string (get-output-string out)))
                    (make-immutable-hash '(("id" . 777)
                                           ("jsonrpc" . "2.0")
                                           ("method" . "foo")
                                           ("params" . (1 2))))))
    (let ((out (open-output-string)))
      (parameterize ((current-jsonrpc-id-generator (make-jsonrpc-id-counter 777))
                     (current-jsonrpc-request-registry (make-jsonrpc-request-registry)))
        (invoke-jsonrpc 'foo out #:params (make-hash '((bar . 1) (baz . 2)))))
      (check-equal? (read-json (open-input-string (get-output-string out)))
                    (make-immutable-hash '(("id" . 777)
                                           ("jsonrpc" . "2.0")
                                           ("method" . "foo")
                                           ("params" . #hash(("bar" . 1) ("baz" . 2))))))))
  (test-case "dispatch-jsonrpc-request"
    (parameterize ((current-jsonrpc-output-port (open-output-string))
                   (current-jsonrpc-id-generator (make-jsonrpc-id-counter 777))
                   (current-jsonrpc-request-registry (make-jsonrpc-request-registry))
                   (current-jsonrpc-method-registry (make-hash)))
      (define-jsonrpc (foo bar baz)
        "This will wrap bar & baz into list and return."
        (list bar baz))
      (invoke-jsonrpc 'foo #:params '(1 2))
      (parameterize ((current-jsonrpc-input-port (open-input-string (get-output-string (current-jsonrpc-output-port)))))
        (check-equal? (dispatch-jsonrpc-request (read-jsonrpc-request)) '(1 2))))))

(define (dispatch-jsonrpc-requests (in (current-jsonrpc-input-port))
                                (out (current-jsonrpc-output-port)))
  (let loop ()
    (let* ((request (read-jsonrpc-request in))
           (id (hash-ref request "id"))
           ;; TODO: bounded worker pool to dispatch requests concurrently
           (result (with-handlers ((exn? identity)) (dispatch-jsonrpc-request request))))
      (if (exn? result)
          (write-jsonrpc-response id out #:error (exn-message result)) ;; TODO: reconstruct continuation marks?
          (write-jsonrpc-response id out #:result result)))
    (loop)))

(define (dispatch-jsonrpc-responses (in (current-jsonrpc-input-port)))
  (let loop ()
    (dispatch-jsonrpc-response (read-jsonrpc-response in))
    (loop)))

(define (call-jsonrpc method
                      (in (current-jsonrpc-input-port))
                      (out (current-jsonrpc-output-port))
                      #:params (params null))
  (let* ((result (async-channel-get (invoke-jsonrpc method #:params params)))
         (result (if (eq? result (current-json-null)) (void) result)))
    (begin0 result
      (when (exn? result)
        (raise result)))))

(define (jsonrpc-tcp-serve host port
                           #:dispatcher (dispatcher dispatch-jsonrpc-requests)
                           #:backlog (backlog 128)
                           #:reuse? (reuse? #t))
  (let* ((listener (tcp-listen port backlog reuse? host))
         (conn-dispatcher (thunk (let-values (((in out) (tcp-accept listener)))
                                   (dispatcher in out))))
         (conn-dispatcher-thread (thread conn-dispatcher)))
    (thunk (kill-thread conn-dispatcher-thread)
           (tcp-close listener))))

(define (jsonrpc-tcp-client host port
                          #:dispatcher (dispatcher dispatch-jsonrpc-responses))
  (let*-values (((in out) (tcp-connect host port))
                ((conn-dispatcher-thread) (thread (thunk (dispatcher in)))))
    (make-jsonrpc-client in out (thunk (kill-thread conn-dispatcher-thread)))))

(define-syntax (with-jsonrpc stx)
  (syntax-parse stx
    ((_ client-expr body ...)
     #'(let ((client client-expr))
         (parameterize ((current-jsonrpc-input-port (jsonrpc-client-in client))
                        (current-jsonrpc-output-port (jsonrpc-client-out client)))
           body ...)))))

(define-syntax (define-jsonrpc-introspection stx)
  (syntax-parse stx
    ((_ (~optional id #:defaults ((id #'introspect))))
     #'(define-jsonrpc (id)
         (for/hash (((name method) (current-jsonrpc-method-registry)))
           (values name (make-hash `((name . ,(jsonrpc-method-name method))
                                     (signature . ,(jsonrpc-method-signature method))
                                     (doc . ,(jsonrpc-method-doc method))))))))))

;;

;; (define-jsonrpc (foo bar baz)
;;   (displayln (list 'foo-called bar baz))
;;   (list bar baz))

;; (define-jsonrpc (foo-void bar baz)
;;   "Proc which returns void"
;;   (displayln (list 'foo-void-called bar baz)))

;; (define-jsonrpc (bar #:baz baz #:qux (qux 'hello))
;;   (displayln (list 'bar-called baz qux))
;;   (make-hash `((baz . ,baz) (qux . ,qux))))

;; (define-jsonrpc-introspection introspect)

;; ;;(current-jsonrpc-method-registry)

;; (define stop (jsonrpc-tcp-serve "127.0.0.1" 9094))
;; (define client (jsonrpc-tcp-client "127.0.0.1" 9094))

;; (stop)
;; (close-jsonrpc-client client)

;; (write-json (with-jsonrpc client
;;               (call-jsonrpc 'introspect)))
