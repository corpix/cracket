#lang racket
(require racket/async-channel
         racket/tcp
         syntax/srcloc
         corpix/json
         corpix/websocket
         (for-syntax corpix/syntax))
(provide define-jsonrpc
         current-jsonrpc-mode
         current-jsonrpc-input-port
         current-jsonrpc-output-port
         current-jsonrpc-id-generator
         current-jsonrpc-method-registry
         current-jsonrpc-request-registry
         current-jsonrpc-read-interceptor
         current-jsonrpc-write-interceptor
         current-jsonrpc-message-field-map
         use-jsonrpc-reflection
         jsonrpc-ref
         jsonrpc-set
         make-jsonrpc-method-registry
         make-jsonrpc-request-registry
         (except-out (struct-out jsonrpc-server) -make-jsonrpc-server)
         (except-out (struct-out jsonrpc-client) -make-jsonrpc-client)
         close-jsonrpc-server
         close-jsonrpc-client
         jsonrpc-tcp-serve
         jsonrpc-tcp-client
         jsonrpc-websocket-serve
         jsonrpc-websocket-client
         with-jsonrpc)
(module+ test
  (require rackunit))

(define-namespace-anchor ns-anchor)

(define-syntax (make-required-parameter stx)
  (syntax-parse stx
    ((_ value error-message)
     #'(make-derived-parameter (make-parameter value) values
                               (lambda (v) (begin0 v (unless v (error error-message))))))))

(define-for-syntax (keyword->symbol value)
  (string->symbol (keyword->string value)))

;;

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

(define-struct jsonrpc-server (closer)
  #:constructor-name -make-jsonrpc-server
  #:transparent)

(define-struct jsonrpc-client (in out closer)
  #:constructor-name -make-jsonrpc-client
  #:transparent)

(define (close-jsonrpc-server server)
  ((jsonrpc-server-closer server)))

(define (close-jsonrpc-client client)
  ((jsonrpc-client-closer client)))

;;

(define (make-jsonrpc-method-registry)
  (make-hash))

(define (make-jsonrpc-request-registry)
  (-make-jsonrpc-request-registry (make-hash)
                                  (make-semaphore 1)))

;;

(define jsonrpc-version "2.0")
(define current-jsonrpc-mode (make-parameter 'strict)) ;; or 'loose
(define current-jsonrpc-input-port (make-required-parameter #f "no input jsonrpc port is active"))
(define current-jsonrpc-output-port (make-required-parameter #f "no output jsonrpc port is active"))
(define current-jsonrpc-id-generator (make-parameter (make-jsonrpc-id-counter)))
(define current-jsonrpc-method-registry (make-parameter (make-jsonrpc-method-registry)))
(define current-jsonrpc-request-registry (make-parameter (make-jsonrpc-request-registry)))
(define current-jsonrpc-read-interceptor (make-parameter #f))
(define current-jsonrpc-write-interceptor (make-parameter #f))
(define current-jsonrpc-message-field-map
  (make-parameter #hasheq((rpc . "jsonrpc")
                          (id . "id")
                          (method . "method")
                          (parameters . "params")
                          (result . "result")
                          (error . "error")
                          (error-code . "code")
                          (error-message . "message")
                          (error-data . "data"))))
(define use-jsonrpc-reflection (make-parameter #f))

;;

(define (make-jsonrpc-server closer)
  (when (use-jsonrpc-reflection)
    (define-jsonrpc-reflection))
  (-make-jsonrpc-server closer))

(define (make-jsonrpc-client in out closer)
  (let ((client (-make-jsonrpc-client in out closer)))
    (begin0 client
      (when (use-jsonrpc-reflection)
        (define-jsonrpc-from client)))))

(define (intercept-jsonrpc-message interceptor message)
  (if interceptor (interceptor message) message))

(define (jsonrpc-ref record field (default #f))
  (hash-ref record (hash-ref (current-jsonrpc-message-field-map) field field)
            default))

(define (jsonrpc-set record field value)
  (hash-set record (hash-ref (current-jsonrpc-message-field-map) field field)
            value))

(define (write-jsonrpc-message id payload (port (current-jsonrpc-output-port)))
  (let ((rpc-payload `((id . ,id) ,@payload)))
    (write-bytes
     ;; NOTE: writing as a single message
     ;; this will help ports like websocket port
     ;; to work correctly (websocket port wrap each write into separate frame)
     ;; TODO: try to rewrite websocket output port to support buffering and write frame when port flushes
     (intercept-jsonrpc-message
      (current-jsonrpc-write-interceptor)
      (json->bytes (make-hasheq (if (eq? (current-jsonrpc-mode) 'strict)
                                    (cons `(jsonrpc . ,jsonrpc-version) rpc-payload)
                                    rpc-payload))))
     port))
  (flush-output port))

(define (write-jsonrpc-request method (port (current-jsonrpc-output-port))
                               #:params (params null)
                               #:next-id (next-id (current-jsonrpc-id-generator)))
  (let ((id (next-id)))
    (begin0 id
      (let* ((payload `((method . ,method)))
             (payload (cond
                        ((pair? params)
                         (cons `(params . ,(for/list ((param (in-list params)))
                                             #:break (void? param)
                                             param))
                               payload))
                        ((hash? params)
                         (cons `(params . ,(for/fold ((acc (make-hash)))
                                                     (((key value) (in-hash params)))
                                             (begin0 acc
                                               (unless (void? value)
                                                 (hash-set! acc key value)))))
                               payload))
                        (else payload))))
        (write-jsonrpc-message id payload port)))))

(define (write-jsonrpc-response id (port (current-jsonrpc-output-port))
                                #:result (result #f)
                                #:error (error #f))
  (let* ((payload (if error
                      `((error . ,error))
                      `((result . ,(if (void? result) 'null result))))))
    (write-jsonrpc-message id payload port)))

(define (read-jsonrpc-message (port (current-jsonrpc-input-port)))
  (let* ((message (intercept-jsonrpc-message (current-jsonrpc-read-interceptor)
                                             (read-json port))))
    (if (eof-object? message) eof
        (begin0 message
          (when (eq? (current-jsonrpc-mode) 'strict)
            (let ((message-version (jsonrpc-ref message 'rpc)))
              (unless message-version
                (error (format "jsonrpc version is not specified in incoming message: ~a" message)))
              (unless (equal? message-version jsonrpc-version)
                (error (format "jsonrpc version mismatch: expected ~s, got ~s" jsonrpc-version message-version)))))))))

(define (read-jsonrpc-request (port (current-jsonrpc-input-port)))
  (let* ((message (read-jsonrpc-message port)))
    (if (eof-object? message) eof
        (begin0 message
          (unless (hash-has-key? message "method")
            (error (format "jsonrpc method is not specified: ~a" message)))))))

(define (read-jsonrpc-response (port (current-jsonrpc-input-port)))
  (let* ((message (read-jsonrpc-message port)))
    (if (eof-object? message) eof message)))

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
    ((_ (name rest ...)
        (~optional (~seq #:method method) #:defaults ((method #'#f)))
        (~optional (~seq #:doc doc) #:defaults ((doc #'#f)))
        (~optional (~seq #:registry registry) #:defaults ((registry #'(current-jsonrpc-method-registry))))
        body ...)
     (with-syntax* ((method-name (or (syntax->datum #'method)
                                     (syntax->datum #'name)))
                    (signature (let loop ((argument-stx #'(rest ...))
                                          (acc null))
                                 (if (pair? (syntax->datum argument-stx))
                                     (syntax-parse argument-stx
                                       (((~seq key:keyword (_ default)) rest ...)
                                        (loop #'(rest ...) (append acc (list #`(keyword (#,(keyword->symbol (syntax->datum #'key))
                                                                                         default))))))
                                       (((~seq key:keyword _) rest ...)
                                        (loop #'(rest ...) (append acc (list #`(keyword (#,(keyword->symbol (syntax->datum #'key))))))))
                                       (((key:id default) rest ...)
                                        (loop #'(rest ...) (append acc (list #'(symbol (key default))))))
                                       ((key:id rest ...)
                                        (loop #'(rest ...) (append acc (list #'(symbol (key)))))))
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
                    ((method-body ...) (cond
                                         ((not (pair? (syntax->list #'(body ...)))) #'((call-jsonrpc 'method-name #:params params)))
                                         (else #'(body ...)))))
       #'(begin
           (define (name rest ...) method-body ...)
           (hash-set! registry (symbol->string 'method-name)
                      (make-jsonrpc-method 'method-name 'signature doc name)))))))

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
  (let* ((method (or (jsonrpc-ref message 'method)
                     (error "method is not specified")))
         (method-name (if (symbol? method) (symbol->string method) method))
         (method-descriptor (hash-ref methods method-name #f))
         (params (jsonrpc-ref message 'parameters null)))
    (unless method-descriptor
      (error (format "method ~s is not defined" method-name)))
    (jsonrpc-apply method-descriptor params)))

(define (dispatch-jsonrpc-response message #:request-registry (registry (current-jsonrpc-request-registry)))
  (if (jsonrpc-ref message 'method #f)
      ;; NOTE: it is a callback
      (dispatch-jsonrpc-request message)
      ;; NOTE: it is a "usual" response
      (let* ((id (jsonrpc-ref message 'id))
             (chan (call-with-semaphore
                    (jsonrpc-request-registry-sem registry)
                    (thunk (hash-ref (jsonrpc-request-registry-channels registry) id #f))))
             (default 'default))
        (let* ((result (jsonrpc-ref message 'result default))
               (err (jsonrpc-ref message 'error default))
               (resp (cond
                       ((not (eq? result default)) result)
                       ((not (eq? err default))
                        (make-exn:fail (string-append
                                        "Remote error: "
                                        (format "(~a) " (jsonrpc-ref err 'error-code #f))
                                        (or (jsonrpc-ref err 'error-message #f)
                                            (format "~a" err))
                                        (let ((data (jsonrpc-ref err 'error-data #f)))
                                          (if data
                                              (format "\nRemote context:\n~a"
                                                      (string-join
                                                       (map (lambda (line) (string-append "  " line))
                                                            data)
                                                       "\n"))
                                              "")))
                                       (current-continuation-marks)))
                       (else (make-exn:fail (format "no result or error present in response: ~a" message)
                                            (current-continuation-marks))))))
          (if chan
              (async-channel-put chan resp)
              (if (exn? resp)
                  (raise resp)
                  (error (format "jsonrpc response dispatch failed, no response channel found for id ~a, response: ~v" id resp))))))))

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
                                           ("params" . #hash(("bar" . 1) ("baz" . 2)))))))
    (let ((out (open-output-string)))
      (parameterize ((current-jsonrpc-id-generator (make-jsonrpc-id-counter 777))
                     (current-jsonrpc-request-registry (make-jsonrpc-request-registry)))
        (invoke-jsonrpc 'foo out #:params (make-hash `((bar . 1313) (baz . 666) (qux . ,(void))))))
      (check-equal? (read-json (open-input-string (get-output-string out)))
                    (make-immutable-hash '(("id" . 777)
                                           ("jsonrpc" . "2.0")
                                           ("method" . "foo")
                                           ("params" . #hash(("bar" . 1313) ("baz" . 666))))))))
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
    (let ((request (read-jsonrpc-request in)))
      (if (eof-object? request) eof
          (let ((id (or (jsonrpc-ref request 'id)
                        (error "request id is not specified")))
                ;; TODO: bounded worker pool to dispatch requests concurrently
                (result (with-handlers ((exn? identity))
                          (dispatch-jsonrpc-request request))))
            (if (exn? result)
                (write-jsonrpc-response
                 id out
                 #:error
                 (make-hasheq `((message . ,(exn-message result))
                                (data . ,(map (lambda (kv)
                                                (string-append (source-location->string (cdr kv)) " "
                                                               (format "~a" (car kv))))
                                              (continuation-mark-set->context (exn-continuation-marks result)))))))
                (write-jsonrpc-response id out #:result result))
            (loop))))))

(define (dispatch-jsonrpc-responses (in (current-jsonrpc-input-port)))
  (let loop ()
    (let ((resp (read-jsonrpc-response in)))
      (if (eof-object? resp) eof
          (begin (dispatch-jsonrpc-response resp)
                 (loop))))))

(define (call-jsonrpc method
                      (out (current-jsonrpc-output-port))
                      #:params (params null))
  (let* ((result (async-channel-get (invoke-jsonrpc method out #:params params)))
         (result (if (eq? result (current-json-null))
                     (void)
                     result)))
    (begin0 result
      (when (exn? result)
        (raise result)))))

(define (make-jsonrpc-dispatcher acceptor #:dispatcher (dispatcher dispatch-jsonrpc-requests))
  (thunk (let-values (((in out) (acceptor)))
           (dispatcher in out))))

;;

(define (jsonrpc-tcp-serve host port
                           #:dispatcher-maker (dispatcher-maker make-jsonrpc-dispatcher)
                           #:backlog (backlog 128)
                           #:reuse? (reuse? #t))
  (let* ((listener (tcp-listen port backlog reuse? host))
         (conn-dispatcher (dispatcher-maker (thunk (tcp-accept listener))))
         (conn-dispatcher-thread (thread conn-dispatcher)))
    (make-jsonrpc-server (thunk (kill-thread conn-dispatcher-thread)
                                (tcp-close listener)))))

(define (jsonrpc-tcp-client host port #:dispatcher (dispatcher dispatch-jsonrpc-responses))
  (let*-values (((in out) (tcp-connect host port))
                ((conn-dispatcher-thread) (thread (thunk (dispatcher in)))))
    (make-jsonrpc-client in out (thunk (kill-thread conn-dispatcher-thread)))))

;;

(define (jsonrpc-websocket-serve host port #:dispatcher (dispatcher dispatch-jsonrpc-requests))
  (let* ((handler (lambda (connection state)
                    (let-values (((in out) (values (make-websocket-input-port connection)
                                                   (make-websocket-output-port connection))))
                      (dispatcher in out))))
         (closer (websocket-serve #:listen-ip host #:port port handler)))
    (make-jsonrpc-server closer)))

(define (jsonrpc-websocket-client url #:dispatcher (dispatcher dispatch-jsonrpc-responses))
  (let*-values (((connection) (websocket-connect url))
                ((in out) (values (make-websocket-input-port connection)
                                  (make-websocket-output-port connection)))
                ((conn-dispatcher-thread) (thread (thunk (dispatcher in)))))
    (make-jsonrpc-client in out (thunk (websocket-close connection)
                                       (kill-thread conn-dispatcher-thread)))))

;;

(define-syntax (with-jsonrpc stx)
  (syntax-parse stx
    ((_ client-expr body ...)
     #'(let ((client client-expr))
         (parameterize ((current-jsonrpc-input-port (jsonrpc-client-in client))
                        (current-jsonrpc-output-port (jsonrpc-client-out client)))
           body ...)))))

(define-syntax (define-jsonrpc-reflection stx)
  (syntax-parse stx
    ((_ (~optional id #:defaults ((id #'reflection))))
     #'(define-jsonrpc (id)
         (for/hash (((name method) (current-jsonrpc-method-registry)))
           (values name (make-hash `((name . ,(jsonrpc-method-name method))
                                     (signature . ,(jsonrpc-method-signature method))
                                     (doc . ,(jsonrpc-method-doc method))))))))))

(define (define-jsonrpc-from client (reflection-method 'reflection))
  (for ((method (hash-values (with-jsonrpc client (call-jsonrpc reflection-method)))))
    (eval `(define-jsonrpc (,(string->symbol (hash-ref method "name"))
                            ,@(for/fold ((acc null))
                                        ((argument (in-list (hash-ref method "signature"))))
                                (let ((kind (car argument))
                                      (expr (cadr argument)))
                                  (case kind
                                    (("symbol") (append acc (list (if (> (length expr) 1)
                                                                      (cons (string->symbol (car expr)) (cdr expr))
                                                                      (string->symbol (car expr))))))
                                    (("keyword") (append acc (list (string->keyword (car expr))
                                                                   (if (> (length expr) 1)
                                                                       (cons (string->symbol (car expr)) (cdr expr))
                                                                       (string->symbol (car expr)))))))))))
          (namespace-anchor->namespace ns-anchor))))

;;

;; (require corpix/jsonrpc)

;; (define-jsonrpc (bar #:baz baz #:qux (qux "hello"))
;;   (displayln (list 'bar-called baz qux))
;;   (make-hash `((baz . ,baz) (qux . ,qux))))

;; ;;

;; (define server (jsonrpc-tcp-serve "127.0.0.1" 9094))
;; (define client (jsonrpc-tcp-client "127.0.0.1" 9094))

;; (with-jsonrpc client (displayln (bar #:baz 1)))

;; (close-jsonrpc-server server)
;; (close-jsonrpc-client client)
