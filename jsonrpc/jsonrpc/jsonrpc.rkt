#lang racket
(require racket/async-channel
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

(define (make-jsonrpc-request-registry)
  (-make-jsonrpc-request-registry (make-hash)
                                  (make-semaphore 1)))

;;

(define jsonrpc-version "2.0")
(define current-jsonrpc-port
  (make-derived-parameter (make-parameter #f)
                          values
                          (lambda (port)
                            (begin0 port
                              (unless port (error "no jsonrpc port is active"))))))

(define current-jsonrpc-id-generator (make-parameter (make-jsonrpc-id-counter)))

(define current-jsonrpc-method-registry (make-parameter (make-hash)))
(define current-jsonrpc-request-registry (make-parameter (make-jsonrpc-request-registry)))

;;

(define (write-jsonrpc-message id payload (port (current-jsonrpc-port)))
  (write-json (make-hasheq `((jsonrpc . ,jsonrpc-version) (id . ,id) ,@payload)) port))

(define (write-jsonrpc-request method (port (current-jsonrpc-port))
                               #:params (params #f)
                               #:next-id (next-id (current-jsonrpc-id-generator)))
  (let ((id (next-id)))
    (begin0 id
      (let* ((payload `((method . ,method)))
             (payload (if params (cons `(params . ,params) payload) payload)))
        (write-jsonrpc-message id payload port)))))

(define (write-jsonrpc-response id (port (current-jsonrpc-port))
                                #:result (result #f)
                                #:error (error #f))
  (let* ((payload (if error
                      `((error . ,error))
                      `((result . ,result)))))
    (write-jsonrpc-message id payload port)))

(define (read-jsonrpc-message (port (current-jsonrpc-port)))
  (let* ((message (read-json port))
         (message-version (hash-ref message "jsonrpc" #f)))
    (begin0 message
      (unless message-version
        (error (format "jsonrpc version is not specified in incoming message: ~a" message)))
      (unless (equal? message-version jsonrpc-version)
        (error (format "jsonrpc version mismatch: expected ~s, got ~s" jsonrpc-version message-version))))))

(define (read-jsonrpc-request (port (current-jsonrpc-port)))
  (let* ((message (read-jsonrpc-message port)))
    (begin0 message
      (unless (hash-has-key? message "method")
        (error (format "jsonrpc method is not specified: ~a" message))))))

(define (read-jsonrpc-response (port (current-jsonrpc-port)))
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
    ((_ (name rest ...) body0 bodyN ...)
     (let ((doc-datum (syntax->datum #'body0)))
       (with-syntax ((name-string (symbol->string (syntax->datum #'name)))
                     (doc (if (string? doc-datum)
                              (datum->syntax stx doc-datum)
                              #'#f))
                     ((body ...) (if (string? doc-datum)
                                     #'(bodyN ...)
                                     #'(body0 bodyN ...))))
         (syntax (begin
                   (define (name rest ...)
                     body ...)
                   (hash-set! (current-jsonrpc-method-registry) name-string
                              (make-jsonrpc-method 'name '(rest ...) doc name)))))))))

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
         (chan (hash-ref registry id #f)))
    (unless chan
      (error (format "jsonrpc response dispatch failed, no response channel found for id ~a" id)))
    (async-channel-put
     chan
     (or (hash-ref message "result" #f)
         (make-exn:fail (hash-ref message "result")
                        (current-continuation-marks))))))

(define (invoke-jsonrpc method (port (current-jsonrpc-port))
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
    (parameterize ((current-jsonrpc-port (open-output-string))
                   (current-jsonrpc-id-generator (make-jsonrpc-id-counter 777))
                   (current-jsonrpc-request-registry (make-jsonrpc-request-registry))
                   (current-jsonrpc-method-registry (make-hash)))
      (define-jsonrpc (foo bar baz)
        "This will wrap bar & baz into list and return."
        (list bar baz))
      (invoke-jsonrpc 'foo #:params '(1 2))
      (let ((in (open-input-string (get-output-string (current-jsonrpc-port)))))
        (check-equal? (dispatch-jsonrpc-request (read-jsonrpc-request in))
                      '(1 2)))))

  )

;; ;;

;; (define-jsonrpc (foo bar baz)
;;   "Returns foo, using positional arguments"
;;   (list 'args=> bar baz))

;; (define-jsonrpc (foo-kw #:bar bar #:baz baz)
;;   "Returns foo, using kw arguments as input"
;;   (list 'args=> bar baz))

;; (current-jsonrpc-method-registry)

;; (dispatch-jsonrpc-request
;;  (make-hash '(("method" . "foo")
;;               ("params" . (1 2)))))
;; (dispatch-jsonrpc-request
;;  (make-hash '(("method" . "foo-kw")
;;               ("params" . #hash(("bar" . 1) (baz . 2))))))

;; {"jsonrpc": "2.0", "method": "subtract", "params": [42, 23], "id": 1}
;; {"jsonrpc": "2.0", "method": "subtract", "params": {"subtrahend": 23, "minuend": 42}, "id": 3}
