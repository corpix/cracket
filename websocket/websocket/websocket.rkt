#lang racket
(require racket/generic
         net/base64
         corpix/http
         ;; FIXME: don't want a dependency from web-server, need to figure this out (implement server in corpix/http?)
         web-server/private/connection-manager
         web-server/private/timer
         web-server/http/request
         web-server/http/request-structs
         web-server/dispatchers/dispatch
         web-server/web-server
         web-server/http/response
         web-server/http/response-structs
         (prefix-in dispatch-sequencer- web-server/dispatchers/dispatch-sequencer)
         (except-in net/url http-connection?)
         (only-in corpix/url current-url-scheme-port-dispatcher) ;; FIXME: move to corpix/url everywhere, for now this is for compat reasons
         (only-in openssl ssl-port? ssl-connect ssl-addresses))
(provide websocket-protocol-name
         websocket-version
         websocket-magic

         current-websocket-stream-buffer-size
         current-websocket-idle-timeout
         current-connection-manager

         websocket-frame-continuation
         websocket-frame-text
         websocket-frame-binary
         websocket-frame-connection-close
         websocket-frame-ping
         websocket-frame-pong

         websocket-payload-text
         websocket-payload-binary
         websocket-payload-continuation

         websocket-frame-type-to-payload-type
         websocket-payload-type-to-frame-type

         websocket-status-normal-closure
         websocket-status-going-away
         websocket-status-protocol-error
         websocket-status-unsupported-data
         websocket-status-reserved
         websocket-status-no-status-received
         websocket-status-abnormal-closure
         websocket-status-invalid-frame-payload
         websocket-status-policy-violation
         websocket-status-message-too-big
         websocket-status-internal-server-error
         websocket-status-service-restart
         websocket-status-try-again-later
         websocket-status-unauthorized

         (struct-out websocket-connection)
         websocket-connection-in
         websocket-connection-out
         (struct-out websocket-protocol-frame)
         websocket-service-mapper

         websocket-send
         websocket-next-frame
         websocket-receive
         websocket-close
         make-websocket-dispatcher
         make-websocket-service-dispatcher
         websocket-http-transport-maker
         websocket-url-scheme-port-dispatcher
         websocket-http-hijacker
         websocket-connect
         make-websocket-input-port
         make-websocket-output-port
         websocket-serve
         websocket-serve*)
(module+ test
  (require rackunit))

;; TODO: cover with (optional?) metrics
;; TODO: clean up names
;; TODO: clean up design (currently it is very messy)

(define websocket-protocol-name #"websocket")
(define websocket-version 13) ;; NOTE: see https://www.iana.org/assignments/websocket/websocket.xml#version-number
(define websocket-magic #"258EAFA5-E914-47DA-95CA-C5AB0DC85B11") ;; NOTE: see https://www.rfc-editor.org/rfc/rfc6455

(define current-websocket-stream-buffer-size (make-parameter 65536))
(define current-websocket-idle-timeout (make-parameter (* 60 10)))
(define current-connection-manager (make-parameter (start-connection-manager)))

(define websocket-frame-continuation 0)
(define websocket-frame-text 1)
(define websocket-frame-binary 2)
(define websocket-frame-connection-close 8)
(define websocket-frame-ping 9)
(define websocket-frame-pong 10)

(define websocket-payload-text 'text)
(define websocket-payload-binary 'binary)
(define websocket-payload-continuation 'continuation)

(define websocket-payload-type-to-frame-type
  (make-hash
   `((,websocket-payload-continuation . ,websocket-frame-continuation)
     (,websocket-payload-text . ,websocket-frame-text)
     (,websocket-payload-binary . ,websocket-frame-binary))))

(define websocket-frame-type-to-payload-type
  (make-hash
   `((,websocket-frame-continuation . ,websocket-payload-continuation)
     (,websocket-frame-text . ,websocket-payload-text)
     (,websocket-frame-binary . ,websocket-payload-binary))))

(define websocket-status-normal-closure 1000)
(define websocket-status-going-away 1001)
(define websocket-status-protocol-error 1002)
(define websocket-status-unsupported-data 1003)
(define websocket-status-reserved 1004)
(define websocket-status-no-status-received 1005)
(define websocket-status-abnormal-closure 1006)
(define websocket-status-invalid-frame-payload 1007)
(define websocket-status-policy-violation 1008)
(define websocket-status-message-too-big 1009)
(define websocket-status-internal-server-error 1011)
(define websocket-status-service-restart 1012)
(define websocket-status-try-again-later 1013)
(define websocket-status-unauthorized 3000)

(define-struct websocket-connection
  ((closed? #:mutable)
   mask?
   path
   headers
   socket
   -in -out)
  #:property prop:evt (struct-field-index -in)
  #:transparent)

(define (websocket-connection-in connection)
  (make-websocket-input-port connection))

(define (websocket-connection-out connection)
  (make-websocket-output-port connection))

;;

(define-struct websocket-protocol-frame
  (final? opcode payload)
  #:transparent)

;;

;; FIXME: move this helpers to some other place or eliminate them completely?
(define (read-int width-in-bytes p)
  (let ((bs (read-bytes width-in-bytes p)))
    (if (eof-object? bs)
        0
        (integer-bytes->integer bs #f #t))))

(define (bytes-xor buf key)
  (define result (make-bytes (bytes-length buf)))
  (for ((b (in-bytes buf))
        (k (in-cycle (in-bytes key)))
        (i (in-naturals)))
    (bytes-set! result i (bitwise-xor b k)))
  result)

(define (not-ready-evt port (value 0))
  (handle-evt port (thunk* value)))

;; FIXME: refactor names
(define (random-key-string)
  (bytes->string/latin-1
   (base64-encode (list->bytes
                   (for/list ((i 16)) ;; FIXME: eliminate magic numbers (make constants)
                     (random 256)))
                  #"")))

(define (key-digest key)
  (base64-encode
   (sha1-bytes
    (open-input-bytes
     (bytes-append key websocket-magic)))
   #""))

;;

(define (reset-connection-timeout! connection)
  (reset-timer! (connection-timer connection)
                (current-websocket-idle-timeout)))

(define (connect in out
                 #:custodian (custodian (make-custodian)))
  (new-connection (current-connection-manager)
                  (current-websocket-idle-timeout)
                  in out custodian #f))

;;

(define-syntax websocket-service-mapper
  (syntax-rules ()
    ((_ (uri-regexp ((protocol ...) function) ...) ...)
     (lambda (uri)
       (define resource (url->resource-string uri #:include-query? #f))
       (cond
         ((regexp-match-exact? uri-regexp resource)
          (lambda (requested-protocol)
            (or (case requested-protocol ((protocol ...) function) (else #f))
                ...)))
         ...
         (else
          (lambda (requested-protocol) #f)))))))

(define (url->resource-string u #:include-query? (include-query? #t)) ;; FIXME: eliminate this requirement, move to corpix/url completely
  (define u1 (struct-copy url u
                          (scheme #f)
                          (user #f)
                          (host #f)
                          (port #f)
                          (fragment #f)))
  (define u2 (if include-query? u1 (struct-copy url u1
                                                (query null))))
  (url->string u2))

(define (lookup-service service-mapper) ;; FIXME: refactor headers management using corpix/http
  (lambda (request-line headers req)
    (define protocol-header (headers-assq* #"sec-websocket-protocol" headers))
    (define requested-protocols (if protocol-header
                                    (map string->symbol
                                         (tokenize-header-value (header-value protocol-header)))
                                    (list #f))) ;; signifies the absence of the header
    (define protocol-mapper (service-mapper (request-uri req)))
    (define-values (selected-protocol proc)
      (let loop ((remaining requested-protocols))
        (match remaining
          ((list) (next-dispatcher))
          ((cons selected-protocol rest)
           (define maybe-function (protocol-mapper selected-protocol))
           (if maybe-function
               (values selected-protocol maybe-function)
               (loop rest))))))
    (values (if selected-protocol
                (list (header #"sec-websocket-protocol"
                              (string->bytes/latin-1 (symbol->string selected-protocol))))
                null)
            proc)))

;;

(define (tokenize-header-value v (value-if-absent null)) ;; FIXME: get rid of this
  (if v
      (map (lambda (p) (string-downcase (string-trim p)))
           (string-split (bytes->string/latin-1 v) ","))
      value-if-absent))

(define (reconstruct-request-line req) ;; FIXME: get rid of this
  (bytes-append (request-method req)
                #" "
                (string->bytes/latin-1 (url->string (request-uri req)))
                #" HTTP/1.1"))

(define (get-header headers k) ;; FIXME: get rid of this
  (let ((h (headers-assq* k headers)))
    (and h (header-value h))))

(define (print-header out h) ;; FIXME: get rid of this
  (fprintf out "~a: ~a\r\n" (header-field h) (header-value h)))

;;

(define (read-frame p)
  (define flags+opcode (read-byte p))
  (define mask+payload-len (read-byte p))
  (define payload-len
    (if (eof-object? mask+payload-len)
        0
        (let ((v (bitwise-and mask+payload-len 127))) ;; FIXME: eliminate magic numbers
          (cond
            ((= v 126) (read-int 2 p)) ;; FIXME: eliminate magic numbers
            ((= v 127) (read-int 8 p))
            (else v)))))
  (define masked? (and (not (eof-object? mask+payload-len))
                       (bitwise-bit-set? mask+payload-len 7)))
  (define masking-key (and masked? (read-bytes 4 p)))
  (define masked-payload (read-bytes payload-len p))
  (define payload (if (and masked?
                           (not (eof-object? masked-payload))
                           (not (eof-object? masking-key)))
                      (bytes-xor masked-payload masking-key)
                      masked-payload))
  (if (or (eof-object? flags+opcode)
          (eof-object? payload))
      eof
      (make-websocket-protocol-frame (bitwise-bit-set? flags+opcode 7) ;; FIXME: eliminate magic numbers
                                     (bitwise-and flags+opcode 15)
                                     payload)))

(define (write-frame f p mask?)
  (match-define (websocket-protocol-frame final? opcode payload) f)
  (write-byte (bitwise-ior (if final? #x80 #x00) (bitwise-and opcode 15)) p)  ;; FIXME: eliminate magic numbers
  (define key (and mask?
                   (bytes (random 256) ;; TODO: cryptographic PRNG/stream cipher?
                          (random 256)
                          (random 256)
                          (random 256))))
  (define payload-length (bytes-length payload))
  (define (compute-length-byte v) (bitwise-ior (if mask? #x80 #x00) v)) ;; FIXME: eliminate magic numbers
  (cond
    ((< payload-length 126) ;; FIXME: eliminate magic numbers
     (write-byte (compute-length-byte payload-length) p))
    ((< payload-length 65536)
     (write-byte (compute-length-byte 126) p)
     (write-byte (arithmetic-shift payload-length -8) p)
     (write-byte (bitwise-and payload-length 255) p))
    (else
     (write-byte (compute-length-byte 127) p)
     (write-bytes (integer->integer-bytes payload-length 8 #f #t) p)))
  (if key
      (begin (write-bytes key p)
             (write-bytes (bytes-xor payload key) p))
      (write-bytes payload p)))

(define (stream-frames in initial-opcode final-fragment? out mask?)
  (define buffer (make-bytes (current-websocket-stream-buffer-size)))
  (let loop ((opcode initial-opcode))
    (match (read-bytes-avail! buffer in)
      ((? eof-object?)
       (when final-fragment?
         (write-frame (make-websocket-protocol-frame #t websocket-frame-continuation #"") out mask?)))
      (fragment-length
       (write-frame (make-websocket-protocol-frame #f opcode (subbytes buffer 0 fragment-length)) out mask?)
       (loop 0)))))

(define (websocket-send connection payload
                        #:final-fragment? (final-fragment? #t)
                        #:payload-type (payload-type websocket-payload-text)
                        #:flush? (flush? #t))
  (unless (websocket-connection-closed? connection)
    (let ((opcode
           (or (hash-ref websocket-payload-type-to-frame-type payload-type #f)
               (error 'websocket-send "unsupported payload type: ~v" payload-type))))
      (if (input-port? payload)
          (stream-frames payload opcode final-fragment?
                         (websocket-connection--out connection)
                         (websocket-connection-mask? connection))
          (let ((payload-bytes (cond
                                 ((bytes? payload) payload)
                                 ((string? payload) (string->bytes/utf-8 payload))
                                 (else (error 'websocket-send "unsupported payload: ~v" payload)))))
            (write-frame (make-websocket-protocol-frame final-fragment? opcode payload-bytes)
                         (websocket-connection--out connection)
                         (websocket-connection-mask? connection)))))
    (when flush?
      (flush-output (websocket-connection--out connection)))))

(define (websocket-next-frame connection)
  (match (read-frame (websocket-connection--in connection))
    ((? eof-object? f) f)
    ((and f (websocket-protocol-frame final? opcode payload))
     (reset-connection-timeout! (websocket-connection-socket connection))
     (cond
       ((or (eq? opcode websocket-frame-continuation)
            (eq? opcode websocket-frame-text)
            (eq? opcode websocket-frame-binary))
        f)
       ((eq? opcode websocket-frame-connection-close)
        (unless (websocket-connection-closed? connection)
          (write-frame (make-websocket-protocol-frame #t websocket-frame-connection-close #"")
                       (websocket-connection--out connection)
                       (websocket-connection-mask? connection))
          (set-websocket-connection-closed?! connection #t))
        eof)
       ((eq? opcode websocket-frame-ping)
        (write-frame (make-websocket-protocol-frame #t websocket-frame-pong payload)
                     (websocket-connection--out connection)
                     (websocket-connection-mask? connection))
        (websocket-next-frame connection))
       ((eq? opcode websocket-frame-pong)
        (websocket-next-frame connection))
       (else
        (websocket-close connection
                         #:status websocket-status-protocol-error
                         #:reason (format "unexpected opcode ~a" opcode))
        eof)))))

(define (websocket-receive connection
                           #:payload-type (payload-type websocket-payload-text))
  (let ((expected-opcode (or (hash-ref websocket-payload-type-to-frame-type payload-type #f)
                             (error 'websocket-receive "unsupported payload type: ~v" payload-type))))
    (let loop ((acc #""))
      (if (websocket-connection-closed? connection)
          eof
          (match (websocket-next-frame connection)
            ((? eof-object?) eof)
            ((websocket-protocol-frame final? opcode payload)
             (unless (equal? opcode expected-opcode)
               (error 'websocket-receive
                      "unexpected opcode ~v, expected ~v for ~v payload type"
                      opcode expected-opcode payload-type))
             (if final?
                 (bytes-append acc payload)
                 (loop (bytes-append acc payload)))))))))

(define (websocket-close connection
                         #:status (status websocket-status-normal-closure)
                         #:reason (reason ""))
  (unless (websocket-connection-closed? connection)
    (set-websocket-connection-closed?! connection #t)
    (with-handlers ((exn:fail? void))
      (write-frame (make-websocket-protocol-frame
                    #t websocket-frame-connection-close
                    (bytes-append (integer->integer-bytes status 2 #f #t)
                                  (string->bytes/utf-8 reason)))
                   (websocket-connection--out connection)
                   (websocket-connection-mask? connection))
      (flush-output (websocket-connection--out connection)))
    (with-handlers ((exn:fail? void))
      (let loop ()
        (unless (eof-object? (websocket-next-frame connection))
          (loop))))
    (close-input-port (websocket-connection--in connection))
    (close-output-port (websocket-connection--out connection))))

(define (make-websocket-dispatcher connection-dispatch (connection-headers #f))
  (lambda (connection req)
    (define headers (request-headers/raw req))
    (define (header-equal? k expected)
      (define actual (get-header headers k))
      (and actual (string-ci=? (bytes->string/latin-1 actual) expected)))
    (define websocket-key (get-header headers #"sec-websocket-key"))
    (when (or (not websocket-key)
              (not (header-equal? #"sec-websocket-version" (number->string websocket-version)))
              (not (member "websocket" (tokenize-header-value (get-header headers #"upgrade"))))
              (not (member "upgrade" (tokenize-header-value (get-header headers #"connection")))))
      (next-dispatcher))
    (define request-line (reconstruct-request-line req))

    (define-values (reply-headers connection-state)
      (if connection-headers
          (if (procedure-arity-includes? connection-headers 3)
              (connection-headers request-line headers req)
              (connection-headers request-line headers))
          (values null (void))))

    (define out (connection-o-port connection))
    (fprintf out "HTTP/1.1 101 switching protocols\r\n")
    (print-header out (header #"upgrade" websocket-protocol-name))
    (print-header out (header #"connection" #"upgrade"))
    (print-header out (header #"sec-websocket-accept" (key-digest websocket-key)))
    (for ((h reply-headers))
      (print-header out h))
    (fprintf out "\r\n")
    (flush-output out)

    (reset-connection-timeout! connection)
    (connection-dispatch (make-websocket-connection #f #f
                                                    request-line
                                                    headers
                                                    connection
                                                    (connection-i-port connection) out)
                         connection-state)))

(define (make-websocket-service-dispatcher service-mapper)
  (dispatch-sequencer-make
   (make-websocket-dispatcher
    (lambda (connection selected-service-handler) (selected-service-handler connection))
    (lookup-service service-mapper))))

(define (websocket-http-transport-maker scheme)
  (match (if (symbol? scheme)
             (symbol->string scheme)
             scheme)
    ("ws" (new http-transport-plaintext%))
    ("wss" (new http-transport-tls%))))

(define websocket-url-scheme-port-dispatcher
  (let* ((t (make-hash))
         (put! (lambda (k v)
                 (begin0 t
                   (hash-set! t k v)))))
    (put! "ws"  80)
    (put! "wss" 443)))

(define (websocket-http-hijacker connection res websocket-key)
  (unless (eq? (http-response-status res) http-status-switching-protocols)
    (error 'websocket-http-hijacker
           (format "expected ~a response status, got: ~a, ~a"
                   http-status-switching-protocols
                   (http-response-status res)
                   (http-response-status-text res))))
  (let ((headers (http-response-headers res)))
    (unless (string-ci=? (http-header-ref headers "upgrade") "websocket")
      (error 'websocket-http-hijacker
             "server did not supply expected upgrade header value for websocket protocol"))
    (unless (string-ci=? (http-header-ref headers "connection") "upgrade")
      (error 'websocket-http-hijacker
             "server did not supply expected connection header value for websocket protocol"))
    (unless (equal? (string-trim (http-header-ref headers "sec-websocket-accept"))
                    (bytes->string/latin-1 (key-digest (string->bytes/latin-1 websocket-key))))
      (error 'websocket-http-hijacker "server supplied an incorrect sec-websocket-accept header value"))
    (values connection headers)))

(define (websocket-connect url #:headers (headers null))
  (let*-values (((websocket-key) (random-key-string))
                ((url) (if (url? url) url (string->url url))) ;; FIXME: we need this because of compatibility issues (should have string & object at same time because corpix/url is not compatible with net/url)
                ((url-string) (if (url? url) (url->string url) url))
                ((connection headers)
                 (parameterize ((current-http-transport-maker websocket-http-transport-maker)
                                (current-url-scheme-port-dispatcher websocket-url-scheme-port-dispatcher))
                   (http url-string
                         #:headers `((connection . "upgrade")
                                     (upgrade . "websocket")
                                     (sec-websocket-key . ,websocket-key)
                                     (sec-websocket-version . ,(number->string websocket-version)))
                         #:hijacker (lambda (connection res)
                                      (websocket-http-hijacker connection res websocket-key)))))
                ((in) (http-connection-in connection))
                ((out) (http-connection-out connection)))
    (make-websocket-connection #f #t
                               (url-path url) headers
                               (connect in out)
                               in out)))

;;

(define (make-websocket-input-port connection
                                   #:payload-type (payload-type websocket-payload-text)
                                   #:on-close (on-close websocket-close))
  (let ((message #f)
        (message-position 0))
    (define (do-read buf)
      (unless message
        (set! message (websocket-receive connection #:payload-type payload-type))
        (set! message-position 0))
      (if (eof-object? message) eof
          (let ((limit (min (- (bytes-length message) message-position)
                            (bytes-length buf))))
            (begin0 limit
              (bytes-copy! buf 0 message message-position limit)
              (set! message-position (+ message-position limit))
              (when (= (bytes-length message) message-position)
                (set! message #f))))))
    (make-input-port (object-name (websocket-connection--in connection)) do-read #f
                     (thunk (on-close connection)))))

(define (make-websocket-output-port connection
                                    #:payload-type (payload-type websocket-payload-text)
                                    #:on-close (on-close websocket-close))
  (define (do-write buf start end)
    (let ((amount (- end start)))
      (begin0 amount
        (when (> amount 0) ;; NOTE: if we know when it flushes... why not buffer whole payload and flush per frame?
          (websocket-send connection (subbytes buf start end) #:payload-type payload-type)))))
  (make-output-port (object-name (websocket-connection--out connection)) always-evt
                    (lambda (buf start end non-block? breakable?) (do-write buf start end))
                    (thunk (on-close connection))
                    #f #f #f #f void 1 (thunk* 'block)))

;;

(define (exn-handler-middleware proc)
  (lambda (connection req)
    (with-handlers ((exn:dispatcher?
		     (lambda (e)
		       (log-info "bad websocket request, ~a ~a"
				 (request-method req)
				 (url->string (request-uri req)))
		       (output-response/method
			connection
			(response http-status-bad-request #"bad websocket request"
                                  (current-seconds) #f null void)
			(request-method req)))))
      (proc connection req))))

;; this is a wrapper for serve proc from web-server which adds websocket related kw arguments
(define websocket-serve
  (procedure-rename
   (make-keyword-procedure
    (lambda (keys vals connection-dispatch . rest)
      (let* ((kvs (map list keys vals))
             (connection-headers-cell (assq '#:connection-headers kvs))
             (connection-headers (and connection-headers-cell (cadr connection-headers-cell)))
             (dispatcher (make-websocket-dispatcher connection-dispatch connection-headers)))
        (match-define (list keys vals) (apply map list (remq connection-headers-cell kvs)))
        (keyword-apply serve
                       (cons '#:dispatch keys)
                       (cons (exn-handler-middleware dispatcher) vals)
                       rest))))
   'websocket-serve))

;; this is a wrapper for serve* proc from web-server which adds websocket related kw arguments
(define websocket-serve*
  (procedure-rename
   (make-keyword-procedure
    (lambda (keys vals service-mapper . rest)
      (keyword-apply serve
		     (cons '#:dispatch keys)
		     (cons (exn-handler-middleware (make-websocket-service-dispatcher service-mapper)) vals)
		     rest)))
   'websocket-serve*))
