#lang racket
(require racket
         (for-syntax racket/syntax)
         (only-in srfi/13 string-index)
         (only-in racket/port port->string)
         (for-syntax racket syntax/parse)
         net/uri-codec
         openssl
         corpix/url
         corpix/multipart
         corpix/hex
         "./http-status.rkt"
         "./http-router.rkt")
(provide (all-from-out "./http-status.rkt")
         (all-from-out "./http-router.rkt")

         http-tls-security

         http-transport<%>
         http-transport-plaintext%
         http-transport-tls%

         current-http-transport-maker
         current-http-transport
         current-http-client
         current-http-user-agent
         current-http-connection-pool

         (except-out (struct-out http-client) -make-http-client)
         (except-out (struct-out http-connection) -make-http-connection)
         (except-out (struct-out http-connection-pool) -make-http-connection-pool)
         (except-out (struct-out http-request) -make-http-request)
         (except-out (struct-out http-response) -make-http-response)

         make-http-client
         make-http-connection-pool
         make-http-transport
         make-limited-input-port
         make-chunked-input-port
         make-http-header
         make-http-headers
         make-default-http-headers
         make-http-response-port
         make-http-request
         make-http-response

         http-connection-pool-put!
         http-connection-pool-get!
         for/http-connection-pool

         http-connection-get!
         http-connection-put!
         http-connect

         http-request-line-encode
         http-request-write
         http-request-send!
         http-response-read!

         http-header-find
         http-header-ref
         http-header-append
         http-header-replace
         http-header-remove
         http-headers?
         http-headers-has
         http-headers-write
         http-headers-read
         http-header-encode
         http-header-decode

         http-encode-method
         http-url-encode-path
         http-url-encode-query
         http-decode-status-line

         http
         ;; http-get
         ;; http-head
         ;; http-post
         ;; http-put
         ;; http-delete
         ;; http-options
         with-request)
(module+ test
  (require rackunit))

(define crlf "\r\n")
(define protocol-version '(1 1))
(define protocol-version-prefix "HTTP/")

(define (read-until-crlf in)
  (read-line in 'return-linefeed))

(define (encode-protocol-version protocol-version)
  (string-append
   protocol-version-prefix
   (string-join (map number->string protocol-version)
                ".")))

(define (decode-protocol-version protocol-version)
  (map string->number
       (string-split
        (string-trim protocol-version protocol-version-prefix
                     #:left? #t
                     #:right? #f)
        ".")))

(define (version-pad v size)
  (let ((lv (length v)))
    (if (< lv size)
        (append v (build-list (- (max size lv) lv) (const 0)))
        v)))

(define (version-take v size)
  (take (version-pad v size) size))

(define (version-parts v1 v2 parts)
  (match parts
    ('major (values (version-take v1 1) (version-take v2 1)))
    ('major+minor (values (version-take v1 2) (version-take v2 2)))
    ('major+minor+patch (values (version-take v1 3) (version-take v2 3)))
    ('all
     (let ((size (max (length v1) (length v2))))
       (values (version-pad v1 size)
               (version-pad v2 size))))))

(define (version-compare v1 v2 #:parts (parts 'all))
  (let-values (((v1 v2) (version-parts v1 v2 parts)))
    (for/fold
        ((r '=))
        ((a v1)
         (b v2))
      #:break (not (eq? r '=))
      (cond ((= a b) '=)
            ((> a b) '>)
            ((< a b) '<)))))

(module+ test
  (test-case "encode-protocol-version"
    (check-equal? (encode-protocol-version '(1 2 3 4)) "HTTP/1.2.3.4"))

  (test-case "decode-protocol-version"
    (check-equal? (decode-protocol-version "HTTP/1.1")   '(1 1))
    (check-equal? (decode-protocol-version "HTTP/1.0")   '(1 0))
    (check-equal? (decode-protocol-version "HTTP/2.1.1") '(2 1 1)))
  (test-case "version-pad"
    (check-equal? (version-pad '()     5) '(0 0 0 0 0))
    (check-equal? (version-pad '(1 2)  5) '(1 2 0 0 0))
    (check-equal? (version-pad '(1 2)  1) '(1 2))
    (check-equal? (version-pad '(1 2) -5) '(1 2)))

  (test-case "version-take"
    (check-equal? (version-take '(1 2 3) 5) '(1 2 3 0 0))
    (check-equal? (version-take '(1 2 3) 2) '(1 2)))

  (test-case "version-parts"
    (check-equal? (let-values (((v1 v2) (version-parts '(1 2) '(4 5 6) 'major)))
                    (list v1 v2))
                  '((1) (4)))
    (check-equal? (let-values (((v1 v2) (version-parts '(1 2) '(4 5 6) 'major+minor)))
                    (list v1 v2))
                  '((1 2) (4 5)))
    (check-equal? (let-values (((v1 v2) (version-parts '(1 2) '(4 5 6) 'major+minor+patch)))
                    (list v1 v2))
                  '((1 2 0) (4 5 6)))
    (check-equal? (let-values (((v1 v2) (version-parts '(1 2 3) '(4 5 6) 'all)))
                    (list v1 v2))
                  '((1 2 3) (4 5 6)))
    (check-equal? (let-values (((v1 v2) (version-parts '(1 2) '(4 5 6) 'all)))
                    (list v1 v2))
                  '((1 2 0) (4 5 6))))

  (test-case "version-compare"
    (check-equal? (version-compare '(1 2 3)   '(1 2 3))                               '=)
    (check-equal? (version-compare '(1 2 3)   '(1 2 4))                               '<)
    (check-equal? (version-compare '(1 2 3)   '(1 2 4)    #:parts 'major)             '=)
    (check-equal? (version-compare '(1 2 3)   '(2 2 4)    #:parts 'major)             '<)
    (check-equal? (version-compare '(3 2 3)   '(2 2 4)    #:parts 'major)             '>)
    (check-equal? (version-compare '(1 2 3)   '(1 2 4)    #:parts 'major+minor)       '=)
    (check-equal? (version-compare '(1 2 3)   '(1 3 4)    #:parts 'major+minor)       '<)
    (check-equal? (version-compare '(1 4 3)   '(1 3 4)    #:parts 'major+minor)       '>)
    (check-equal? (version-compare '(1 2 4 1) '(1 2 4 6)  #:parts 'major+minor+patch) '=)
    (check-equal? (version-compare '(1 2 3 1) '(1 2 4 1)  #:parts 'major+minor+patch) '<)
    (check-equal? (version-compare '(1 2 5 1) '(1 2 4 1)  #:parts 'major+minor+patch) '>)))

;;

(define (not-ready-evt port (value 0))
  (handle-evt port (thunk* value)))

(define (find-bytes buf seq (offset 0))
  (let ((found? #f)
        (pos     0))
    (for ((n (in-list (bytes->list buf)))
          #:break found?)
      (cond ((and (< offset (bytes-length seq))
                  (eq? n (bytes-ref seq offset)))
             (set! offset (add1 offset))
             (when (= offset (bytes-length seq))
               (set! found? #t)))
            (else (set! offset 0)))
      (set! pos (add1 pos)))
    ;; XXX: pos is a "border" of the seq
    ;; will not subtract seq length from pos
    ;; because negative values are not a good thing
    ;; when you searching for some byte sequence
    ;; in the `in` port.
    (values found? pos offset)))

;; FIXME: this should not be a macro (case-lambda is enough)
(define-syntax (expand-bytes stx)
  (syntax-case stx ()
    ((_ buf)              #'(expand-bytes buf (lambda (b) (* 2 (bytes-length b)))))
    ((_ buf get-new-size) #'(let ((new-buf (make-bytes (get-new-size buf))))
                              (bytes-copy! new-buf 0 buf 0)
                              new-buf))))

(module+ test
  (test-case "find-bytes"
    (check-equal? (let-values (((found? pos offset)
                                (find-bytes #"abc\r\ncba" #"\r\n")))
                    (list found? pos offset))
                  (list #t 5 2))

    (check-equal? (let-values (((found? pos offset)
                                (find-bytes #"abc\r\ncba" #"OOPS")))
                    (list found? pos offset))
                  (list #f 8 0))

    (check-equal? (let ((offset 0))
                    (for/fold ((acc null))
                              ((v (in-list (list #"abc\r" #"\nfoo"))))
                      (let-values (((found? pos new-offset)
                                    (find-bytes v #"\r\n" offset)))
                        (set! offset new-offset)
                        (cons (list found? pos offset) acc))))
                  '((#t 1 2) (#f 4 1))))

  (test-case "expand-bytes"
    (check-equal? (expand-bytes (make-bytes 2)) (make-bytes 4))
    (check-equal? (expand-bytes (make-bytes 2)
                                (lambda (buf) (* 3 (bytes-length buf))))
                  (make-bytes 6))))

;;

(define (make-limited-input-port port limit #:on-close (on-close close-input-port))
  (let ((got 0)
        (semaphore (make-semaphore 1)))

    (define (do-read buf)
      (let ((count (min (- limit got) (bytes-length buf))))
        (if (zero? count)
            eof
            (let ((n (read-bytes-avail!* buf port 0 count)))
              (cond ((eq?     n 0) (not-ready-evt port))
                    ((number? n)   (set! got (+ got n)) n)
                    (else n))))))

    (define (do-close port)
      (let ((rest (- limit got)))
        (read-bytes rest port)
        (set! got limit))
      (on-close port))

    (define (try-again)
      (not-ready-evt
       (semaphore-peek-evt semaphore)))

    (make-input-port
     (object-name port)
     (lambda (buf)
       (call-with-semaphore
        semaphore
        do-read
        try-again
        buf))
     #f
     (thunk
      (call-with-semaphore
       semaphore
       do-close
       try-again
       port)))))

(module+ test
  (test-case "make-limited-input-port"
    (let* ((raw-in (open-input-string "hello world!"))
           (first-closed? #f)
           (second-closed? #f)
           (first-in (make-limited-input-port
                      raw-in 6
                      #:on-close
                      (lambda (port) (set! first-closed? #t))))
           (second-in (make-limited-input-port
                       raw-in 5
                       #:on-close
                       (lambda (port) (set! second-closed? #t)))))
      (check-equal? (read-string 2 first-in) "he")
      (close-input-port first-in)
      (check-true first-closed?)
      (check-equal? (port->string second-in) "world")
      (check-true (eof-object? (read-string 1 second-in)))
      (close-input-port second-in)
      (check-true second-closed?)
      (check-equal? (port->string raw-in) "!")
      (check-true (eof-object? (read-string 1 raw-in))))
    (let* ((raw-in (open-input-string "hello world!"))
           (limited-in (make-limited-input-port raw-in 6)))
      (check-equal? (peek-string 2 0 limited-in) "he")
      (check-equal? (read-string 2 limited-in) "he")
      (check-equal? (peek-string 2 0 limited-in) "ll")
      (check-equal? (read-string 2 limited-in) "ll"))))

(define (make-chunked-input-port port
                                 #:delimiter (delimiter #"\r\n")
                                 #:on-close  (on-close  close-input-port))

  (define (get-port-progress-evt port)
    (if (port-provides-progress-evts? port)
        (port-progress-evt port)
        #f))

  (let ((semaphore (make-semaphore 1))
        (size 0)
        (got 0)
        (state 'read-size)
        (buf (make-bytes 4))
        (offset 0)
        (dpos 0)
        (cursor 0))

    (define do-read-size
      (let ((buf (make-bytes 4))
            (offset 0)
            (dpos 0)
            (cursor 0))
        (thunk
         ;; FIXME: it is a good idea to have "size limit" here
         (when (= offset (bytes-length buf)) (set! buf (expand-bytes buf)))
         (let ((n (peek-bytes-avail!* buf 0 (get-port-progress-evt port) port offset (bytes-length buf))))
           (cond ((eq? n 0) (not-ready-evt port))
                 ((number? n)
                  (let*-values (((found? pos new-dpos)
                                 (find-bytes (subbytes buf offset (+ offset n)) delimiter dpos)))
                    (when (port-commit-peeked pos (get-port-progress-evt port) always-evt port)
                      (cond (found?
                             (set! size (hex-string->integer
                                         (bytes->string/latin-1
                                          (subbytes buf 0 (- (+ cursor pos)
                                                             (bytes-length delimiter))))))
                             (set! offset 0)
                             (set! cursor 0)
                             (set! dpos 0)
                             (set! state 'read-chunk))
                            (else
                             (set! offset (+ offset n))
                             (set! cursor (+ cursor pos))
                             (set! dpos new-dpos)))))
                  (not-ready-evt port))
                 (else n))))))

    (define (do-read-chunk buf)
      (if (zero? size)
          (begin
            (set! state 'eof)
            ;; discard \r\n in the tail of the "zero"(last) chunk
            (not-ready-evt (read-bytes-evt (bytes-length delimiter) port)))
          (let ((count (max 0 (min (- size got) (bytes-length buf)))))
            (if (zero? count)
                (begin
                  (set! got 0)
                  (set! size 0)
                  (set! state 'read-size)
                  ;; discard \r\n in the tail of the chunk
                  (not-ready-evt (read-bytes-evt (bytes-length delimiter) port)))
                (let ((n (read-bytes-avail!* buf port 0 count)))
                  (cond ((eq?        n 0) (not-ready-evt port))
                        ((number?    n)   (set! got (+ got n)) n)
                        ((procedure? n)   (set! got (add1 got)) n)
                        (else             n)))))))

    (define (do-read buf)
      (match state
        ('read-chunk (do-read-chunk buf))
        ('read-size  (do-read-size))
        ('eof        eof)))

    (define (do-close port)
      (define (wait e)
        (and (evt? e) (sync e)))

      (when (not (eq? state 'eof))
        (let loop ((buf (make-bytes 128)))
          (cond ((eq? state 'read-chunk) (wait (do-read-chunk buf))
                                         (loop buf))
                ((eq? state 'read-size)  (wait (do-read-size))
                                         (loop buf)))))
      (on-close port))

    (define (try-again)
      (not-ready-evt (semaphore-peek-evt semaphore)))

    (make-input-port
     (object-name port)
     (lambda (buf)
       (call-with-semaphore
        semaphore
        do-read
        try-again
        buf))
     #f
     (thunk
      (call-with-semaphore
       semaphore
       do-close
       try-again
       port)))))

(module+ test
  (let* ((raw-in (open-input-string "0\r\n\r\n"))
         (in     (make-chunked-input-port raw-in)))
    (check-equal? (read-char in) eof))
  (let* ((raw-in (open-input-string "0\r\n\r\nwat"))
         (in     (make-chunked-input-port raw-in)))
    (check-equal? (read-char in) eof))
  (let* ((raw-in (open-input-string "a\r\n1234567890\r\n0\r\n\r\n"))
         (in     (make-chunked-input-port raw-in)))
    (check-equal? (port->string in) "1234567890"))
  (let* ((raw-in (open-input-string "a\r\n1234567890\r\nb\r\n12345678901\r\n0\r\n\r\n"))
         (in     (make-chunked-input-port raw-in)))
    (check-equal? (port->string in) "123456789012345678901"))
  (let* ((raw-in (open-input-string "a\r\n1234567890\r\n0\r\n\r\n"))
         (in     (make-chunked-input-port raw-in)))
    (check-equal? (peek-string 10 0 in) "1234567890")
    (check-equal? (peek-string 1 10 in)  eof)
    (check-equal? (read-string 10   in) "1234567890")
    (check-equal? (read-string 1    in)  eof))
  (let* ((raw-in (open-input-string "a\r\n1234567890\r\nb\r\n12345678901\r\n0\r\n\r\n"))
         (in     (make-chunked-input-port raw-in)))
    (check-equal? (peek-string 11 0  in) "12345678901")
    (check-equal? (peek-string 10 11 in) "2345678901")
    (check-equal? (peek-string 1  21 in)  eof)
    (check-equal? (read-string 10    in) "1234567890")
    (check-equal? (read-string 11    in) "12345678901")
    (check-equal? (read-string 1     in)  eof)))

;;

(define-struct http-connection-pool (store semaphore)
  #:transparent
  #:constructor-name -make-http-connection-pool)
(define (make-http-connection-pool . values)
  (-make-http-connection-pool (make-hash values)
                              (make-semaphore 1)))
(define current-http-connection-pool (make-parameter (make-http-connection-pool)))

(define (http-connection-pool-get! pool key (default #f))
  (define store (http-connection-pool-store pool))
  (call-with-semaphore
   (http-connection-pool-semaphore pool)
   (thunk
    (let ((bucket (hash-ref store key #f)))
      (if (and bucket (not (null? bucket)))
          (begin
            (hash-set! store key (cdr bucket))
            (car bucket))
          default)))))

(define (http-connection-pool-put! pool key connection)
  (define store (http-connection-pool-store pool))
  (call-with-semaphore
   (http-connection-pool-semaphore pool)
   (thunk
    (hash-set! store key
               (cons connection
                     (hash-ref store key null))))))

;; FIXME: eliminate syntax template duplication
(define-syntax (for/http-connection-pool stx)
  (syntax-case stx ()
    ((_ (((k v) pool)) body0 body ...)
     #'(let* ((p pool)
              (s (http-connection-pool-store p)))
         (call-with-semaphore
          (http-connection-pool-semaphore p)
          (thunk
           (for ((k (in-list (hash-keys s))))
             (for ((v (in-list (hash-ref s k))))
               body0 body ...))))))
    ((_ ((v pool)) body0 body ...)
     #'(let* ((p pool)
              (s (http-connection-pool-store p)))
         (call-with-semaphore
          (http-connection-pool-semaphore p)
          (thunk
           (for ((k (in-list (hash-keys s))))
             (for ((v (in-list (hash-ref s k))))
               body0 body ...))))))))

(module+ test
  (test-case "pool-get!"
    (let ((pool (make-http-connection-pool)))
      (check-equal? (http-connection-pool-get! pool 1) #f)
      (check-equal? (http-connection-pool-get! pool 1 #f) #f))
    (let ((pool (make-http-connection-pool)))
      (http-connection-pool-put! pool 1 #t)
      (check-equal? (http-connection-pool-get! pool 1) #t)
      (check-equal? (http-connection-pool-get! pool 1 #f) #f)))

  (test-case "pool-put!"
    (let ((pool (make-http-connection-pool)))
      (http-connection-pool-put! pool 1 2)
      (check-equal? (http-connection-pool-store pool)
                    (make-hash '((1 2))))
      (http-connection-pool-put! pool 3 4)
      (check-equal? (http-connection-pool-store pool)
                    (make-hash '((1 2) (3 4))))
      (http-connection-pool-put! pool 1 9)
      (check-equal? (http-connection-pool-store pool)
                    (make-hash '((1 9 2) (3 4))))
      (http-connection-pool-put! pool 1 10)
      (check-equal? (http-connection-pool-store pool)
                    (make-hash '((1 10 9 2) (3 4))))))

  (test-case "for/pool"
    (let ((acc null))
      (for/http-connection-pool ((v (make-http-connection-pool)))
        (set! acc (cons v acc)))
      (check-equal? acc null))
    (let ((acc null)
          (pool (make-http-connection-pool)))
      (for ((n (reverse '(1 2 3 4))))
        (http-connection-pool-put! pool 'first n))
      (for ((n (reverse '(5 6 7 8))))
        (http-connection-pool-put! pool 'second n))
      (for/http-connection-pool ((v pool))
        (set! acc (cons v acc)))
      (check-equal? (sort acc >) '(8 7 6 5 4 3 2 1)))
    (let ((acc null)
          (pool (make-http-connection-pool)))
      (for ((n (reverse '(1 2 3 4))))
        (http-connection-pool-put! pool 'first n))
      (for ((n (reverse '(5 6 7 8))))
        (http-connection-pool-put! pool 'second n))
      (for/http-connection-pool (((k v) pool))
        (set! acc (cons (list k v) acc)))
      (check-equal? (sort acc (lambda (l r) (> (cadr l)
                                               (cadr r))))
                    '((second 8)
                      (second 7)
                      (second 6)
                      (second 5)
                      (first 4)
                      (first 3)
                      (first 2)
                      (first 1))))))

;;

(define-struct http-connection
  (transport host port in out)
  #:transparent
  #:constructor-name -make-http-connection)

(define http-tls-security (make-parameter 'secure))

(define http-transport<%> (interface () get-name connect))

(define http-transport-plaintext%
  (class* object% (http-transport<%>)
    (super-new)
    (define/public (get-name) 'plaintext)
    (define/public (connect host port)
      (tcp-connect host port))))

(define http-transport-tls%
  (class* object% (http-transport<%>)
    (super-new)
    (define/public (get-name) 'tls)
    (define/public (connect host port)
      (ssl-connect host port (http-tls-security)))))

(define current-http-transport-maker (make-parameter #f))

(define (make-http-transport scheme)
  (let ((maker (current-http-transport-maker)))
    (if maker
        (maker scheme)
        (match (if (symbol? scheme)
                   (symbol->string scheme)
                   scheme)
          ("http" (new http-transport-plaintext%))
          ("https" (new http-transport-tls%))))))

(define current-http-transport (make-parameter (make-http-transport 'http)))

(define (http-connect host port
                      #:transport (transport (current-http-transport)))
  (let-values (((in out) (send transport connect host port)))
    (-make-http-connection transport
                           host port
                           in out)))

(define (make-connection-key host port
                             #:transport (transport (current-http-transport)))
  (format "~a-~a-~a"
          (send transport get-name)
          host port))

(define (http-connection-get! host port
                              #:pool (pool (current-http-connection-pool))
                              #:transport (transport (current-http-transport)))
  (or (let* ((key (make-connection-key host port #:transport transport))
             (connection (http-connection-pool-get! pool key)))
        (and connection
             (match (sync/timeout 0 (peek-bytes-evt 1 0 #f (http-connection-in connection)))
               ((? eof-object?) #f) ;; connection timeout or closed
               (_ connection))))
      (http-connect host port #:transport transport)))

(define (http-connection-put! connection #:pool (pool (current-http-connection-pool)))
  (http-connection-pool-put! pool
                             (make-connection-key (http-connection-host connection)
                                                  (http-connection-port connection)
                                                  #:transport (http-connection-transport connection))
                             connection))

(module+ test
  (define test-transport%
    (class* object% (http-transport<%>)
      (super-new)
      (define/public (get-name) 'test)
      (define/public (connect host port)
        (values (open-input-string "123477777777777777777777777777777777777777")
                (open-output-string)))))

  (test-case "make-connection-key"
    (let ((transport (new (class object%
                            (super-new)
                            (define/public (get-name) 'test)))))
      (check-equal? (make-connection-key "example.com" 666
                                         #:transport transport)
                    "test-example.com-666")))

  (test-case "connection-get!"
    (let ((transport (new test-transport%))
          (pool      (make-http-connection-pool)))
      (check-true (http-connection? (http-connection-get! "example.com" 666
                                                          #:pool pool
                                                          #:transport transport))))
    (let* ((transport  (new test-transport%))
           (connection (http-connect "example.com" 666 #:transport transport))
           (key        (make-connection-key "example.com" 666
                                            #:transport transport))
           (pool       (make-http-connection-pool)))
      (http-connection-pool-put! pool key connection)
      (let ((connection-from-pool (http-connection-get! "example.com" 666
                                                        #:pool pool
                                                        #:transport transport)))
        (check-eq? (http-connection-transport connection-from-pool) transport)
        (check-eq? (http-connection-in connection-from-pool) (http-connection-in connection))
        (check-eq? (http-connection-out connection-from-pool) (http-connection-out connection)))
      (let ((connection-from-pool (http-connection-get! "example.com" 666
                                                        #:pool pool
                                                        #:transport transport)))
        (check-not-eq? connection-from-pool connection)
        (check-eq? (http-connection-transport connection-from-pool) transport))))

  (test-case "connection-put!"
    (let* ((transport  (new test-transport%))
           (connection (http-connect "example.com" 666 #:transport transport))
           (pool       (make-http-connection-pool)))
      (http-connection-put! connection
                            #:pool pool)
      (let ((connection-from-pool (http-connection-get! "example.com" 666
                                                        #:pool pool
                                                        #:transport transport)))
        (check-eq? (http-connection-transport connection-from-pool) transport)
        (check-eq? (http-connection-in connection-from-pool) (http-connection-in connection))
        (check-eq? (http-connection-out connection-from-pool) (http-connection-out connection)))
      (let ((connection-from-pool (http-connection-get! "example.com" 666
                                                        #:pool pool
                                                        #:transport transport)))
        (check-not-eq? connection connection-from-pool)
        (check-eq? (http-connection-transport connection-from-pool) transport)))))

;;

(define-struct http-client
  (pool)
  #:transparent
  #:constructor-name -make-http-client)

(define/contract (make-http-client (pool (current-http-connection-pool)))
  (-> http-client?)
  (-make-http-client pool))

(define current-http-client (make-parameter (make-http-client)))

;;

(define current-http-user-agent (make-parameter "corpix-http/1.0"))

(define (make-http-header header)
  (cons (let ((k (car header)))
          (if (symbol? k)
              (symbol->string k)
              k))
        (cdr header)))

(define (make-http-headers . headers)
  (map make-http-header headers))

(define (make-default-http-headers host)
  (make-http-headers `(Host . ,host)
                     `(User-Agent . ,(current-http-user-agent))
                     '(Accept . "*/*")
                     '(Connection . "close")))

(define (-http-header-find headers name (default #f))
  (let ((name (if (symbol? name)
                  (symbol->string name)
                  name)))
    (for/fold ((index 0)
               (value #f))
              ((header headers))
      #:break value
      (if (string-ci=? (car header) name)
          (values index header)
          (values (+ 1 index) #f)))))

(define (http-header-find-index headers name)
  (let-values (((index header) (-http-header-find headers name)))
    (and header index)))

(define (http-header-find headers name (default #f))
  (let-values (((_ header) (-http-header-find headers name)))
    (or header default)))

(define (http-header-ref headers name (default #f))
  (let ((header (http-header-find headers name)))
    (if header (cdr header) default)))

(define (http-header-append headers . rest)
  (apply append headers (list (map make-http-header rest))))

(define (http-header-replace headers . rest)
  (for/fold ((headers headers))
            ((header (in-list rest)))
    (let* ((header (make-http-header header))
           (index (http-header-find-index headers (car header))))
      (if index
          (let-values (((left right) (split-at headers index)))
            (append left
                    (list (make-http-header header))
                    (cdr right)))
          (append headers (list (make-http-header header)))))))

;; (http-header-replace (list (cons "xxx" "yyy") (cons "foo" "bar")) '(foo . "baz"))

(define (http-header-remove headers . rest)
  (for/fold ((headers headers))
            ((name (in-list rest)))
    (let ((index (http-header-find-index headers name)))
      (if index
          (let-values (((left right) (split-at headers index)))
            (append left (drop right 1)))
          headers))))

;; (http-header-remove (list (cons "xxx" "yyy") (cons "foo" "bar")) 'foo)

(define (http-headers? v)
  ((listof (cons/c string? string?)) v))

(define (http-headers-has headers header)
  (let* ((header (make-http-header header))
         (found (http-header-ref headers (car header))))
    (and found (equal? (cdr header) found))))

(define (http-headers-write headers (out (current-output-port)))
  (for ((header headers))
    (write-string (http-header-encode header) out)
    (write-string crlf out)))

(define (http-headers-read (in (current-input-port)))
  (let loop ((acc null))
    (define v (read-until-crlf in))
    (if (or (eof-object? v)
            (= (string-length v) 0))
        (reverse acc)
        (loop (cons (http-header-decode v)
                    acc)))))

(define (http-header-encode header)
  (let ((header (make-http-header header)))
    (string-append (car header)
                   ": "
                   (cdr header))))

(define (http-header-decode header)
  (let ((index (string-index header #\:)))
    (when (not index)
      (error "header delimiter was not found"))
    (cons
     (string-trim (substring header 0 index))
     (string-trim (substring header (add1 index))))))

(module+ test
  (test-case "make-http-headers"
    (check-equal? (make-http-headers '(foo . "bar")
                                     '(baz . "qux"))
                  (list (cons "foo" "bar")
                        (cons "baz" "qux"))))

  (test-case "make-default-http-headers"
    (check-true ((listof (cons/c string? string?))
                 (make-default-http-headers "example.com")))
    (check-equal? (assoc "Host" (make-default-http-headers "example.com"))
                  '("Host" . "example.com")))

  (test-case "http-header-find"
    (check-equal? (http-header-find (make-http-headers '(Host . "example.com"))
                                    'host)
                  '("Host" . "example.com"))
    (check-equal? (http-header-find (make-http-headers '(Connection . "keep-alive"))
                                    'Connection)
                  '("Connection" . "keep-alive"))
    (check-equal? (http-header-find (make-http-headers '(Host . "example.com")
                                                       '(Connection . "keep-alive"))
                                    'Connection)
                  '("Connection" . "keep-alive"))
    (check-equal? (http-header-find (make-http-headers '(Host . "example.com")
                                                       '(Connection . "keep-alive")
                                                       '(Connection . "close"))
                                    'Connection)
                  '("Connection" . "keep-alive"))

    (check-equal? (http-header-find (make-http-headers '(Host . "example.com")) 'f) #f)
    (check-equal? (http-header-find (make-http-headers) 'f) #f)
    (check-equal? (http-header-find (make-http-headers) 'f (void)) (void)))

  (test-case "http-header-ref"
    (check-equal? (http-header-ref (list '("Connection" . "keep-alive")) 'Connection)
                  "keep-alive"))

  (test-case "http-header-append"
    (check-equal? (http-header-append (make-http-headers '(Connection . "keep-alive"))
                                      '(Cache-Control . "no-cache"))
                  (make-http-headers '(Connection . "keep-alive")
                                     '(Cache-Control . "no-cache")))
    (check-equal? (http-header-append (make-http-headers '(Connection "keep-alive"))
                                      '(Cache-Control "no-cache")
                                      '(Content-Type "text/html"))
                  (make-http-headers '(Connection "keep-alive")
                                     '(Cache-Control "no-cache")
                                     '(Content-Type "text/html"))))

  (test-case "http-header-replace"
    (check-equal? (http-header-replace (make-http-headers '(Connection . "keep-alive"))
                                       '(Connection . "close"))
                  (make-http-headers '(Connection . "close")))
    (check-equal? (http-header-replace (make-http-headers) '(Host . "example.com"))
                  (make-http-headers '(Host . "example.com")))
    (check-equal? (http-header-replace '()
                                       '(Connection . "close")
                                       '(Connection . "keep-alive"))
                  '(("Connection" . "keep-alive")))
    (check-equal? (http-header-replace '()
                                       '(Hello . "me1")
                                       '(Bar . "baz"))
                  '(("Hello" . "me1")
                    ("Bar" . "baz")))
    (check-equal? (http-header-replace (make-http-headers '(Hello . "you")
                                                          '(Foo . "bar"))
                                       '(Hello . "me2")
                                       '(Bar . "baz"))
                  '(("Hello" . "me2")
                    ("Foo" . "bar")
                    ("Bar" . "baz")))
    (check-equal? (http-header-replace (make-http-headers '(hello . "you"))
                                       '(Hello . "me3")
                                       '(Bar . "baz"))
                  '(("Hello" . "me3") ("Bar" . "baz"))))

  (test-case "http-header-remove"
    (check-equal? (http-header-remove (make-http-headers '(Connection . "keep-alive"))
                                      'Connection)
                  (make-http-headers))
    (check-equal? (http-header-remove (make-http-headers '(Connection . "keep-alive")
                                                         '(Connection . "close"))
                                      'Connection)
                  (make-http-headers '(Connection . "close")))
    (check-equal? (http-header-remove (make-http-headers '(Connection . "keep-alive")
                                                         '(Cache-Control . "no-cache"))
                                      'Connection)
                  (make-http-headers '(Cache-Control . "no-cache")))
    (check-equal? (http-header-remove (make-http-headers '(Connection . "keep-alive")
                                                         '(Cache-Control . "no-cache"))
                                      'Connection
                                      'Cache-Control)
                  (make-http-headers)))

  (test-case "http-headers-has"
    (check-equal? (http-headers-has (make-http-headers '(Foo . "bar"))
                                    '(Foo . "bar"))
                  #t)
    (check-equal? (http-headers-has (make-http-headers '(Foo . "bar"))
                                    '(foo . "bar"))
                  #t)
    (check-equal? (http-headers-has (make-http-headers '(Foo . "bar"))
                                    '(Bar . "baz"))
                  #f))

  (test-case "http-headers-write"
    (let ((out (open-output-string)))
      (check-equal? (begin
                      (http-headers-write (make-http-headers '(Host . "localhost")) out)
                      (get-output-string out))
                    "Host: localhost\r\n"))
    (let ((out (open-output-string)))
      (check-equal? (begin
                      (http-headers-write (make-http-headers '(Host . "localhost")
                                                             '(User-Agent . "test"))
                                          out)
                      (get-output-string out))
                    "Host: localhost\r\nUser-Agent: test\r\n"))

    (let ((out (open-output-string)))
      (check-equal? (begin
                      (http-headers-write (make-http-headers '(Set-Cookie . "name1=value; HttpOnly")
                                                             '(Set-Cookie . "name2=value; HttpOnly"))
                                          out)
                      (get-output-string out))
                    "Set-Cookie: name1=value; HttpOnly\r\nSet-Cookie: name2=value; HttpOnly\r\n")))

  (test-case "http-headers-read"
    (let ((in (open-input-string "Foo: bar\r\nBaz: qux")))
      (check-equal? (http-headers-read in)
                    '(("Foo" . "bar") ("Baz" . "qux"))))
    (let ((in (open-input-string "Foo: bar\r\nBaz: qux\r\n\r\n")))
      (check-equal? (http-headers-read in)
                    '(("Foo" . "bar") ("Baz" . "qux")))))

  (test-case "http-header-decode"
    (check-equal? (http-header-decode "Foo: bar")
                  '("Foo" . "bar"))
    (check-equal? (http-header-decode "Foo:bar")
                  '("Foo" . "bar"))
    (check-equal? (http-header-decode "Foo  :  bar")
                  '("Foo" . "bar"))))

;;

(define-struct http-request
  (host port method path query headers form multipart body)
  #:transparent
  #:constructor-name -make-http-request)

(define/contract (make-http-request host port path
                                    #:method (method 'get)
                                    #:query (query #f)
                                    #:headers (headers #f)
                                    #:form (form #f)
                                    #:multipart (multipart #f)
                                    #:body (body #f))
  (->* (string? exact-nonnegative-integer? string?)
       (#:method symbol?
        #:query (or/c false/c (listof (cons/c symbol? string?)))
        #:headers (or/c false/c (listof (cons/c symbol? string?)))
        #:form (or/c false/c (listof (cons/c symbol? any/c)))
        #:multipart (or/c false/c (listof (cons/c symbol? any/c)))
        #:body (or/c false/c string? bytes? input-port?))
       http-request?)
  (let* ((headers (if (not headers)
                      (make-default-http-headers host)
                      (apply http-header-replace (make-default-http-headers host) headers)))
         (body (cond (form (let* ((data (string->bytes/utf-8 (alist->form-urlencoded form))))
                             (set! headers (http-header-replace
                                            headers
                                            '(Content-Type . "application/x-www-form-urlencoded")
                                            `(Content-Length . ,(number->string (bytes-length data)))))
                             (open-input-bytes data)))
                     (multipart (let* ((boundary (multipart-form-random-boundary))
                                       (form (make-multipart-form boundary multipart))
                                       ;; FIXME: no need to read it into memory if you could
                                       ;; know where is a file. Because this file could be then stat'ed
                                       ;; and we could stream multipart upload
                                       (data (multipart-form->bytes form)))
                                  (set! headers (http-header-replace
                                                 headers
                                                 `(Content-Type . ,(multipart-form-content-type boundary))
                                                 `(Content-Length . ,(number->string (bytes-length data)))))
                                  (open-input-bytes data)))
                     (body (cond
                             ((input-port? body) body)
                             ((string? body)
                              (let ((body-bytes (string->bytes/utf-8 body)))
                                (set! headers
                                  (http-header-replace
                                   headers
                                   `(Content-Length . ,(number->string (bytes-length body-bytes)))))
                                (open-input-bytes (string->bytes/utf-8 body))))
                             ((bytes? body)
                              (set! headers
                                (http-header-replace
                                 headers
                                 `(Content-Length . ,(number->string (bytes-length body)))))
                              (open-input-bytes body))))
                     (else #f))))
    (-make-http-request
     host port
     method path
     query
     headers
     form multipart body)))

(define (http-encode-method method)
  (string-upcase (symbol->string method)))

(define (http-url-encode-path path)
  (url-encode path 'path))

(define (http-url-encode-query query)
  (alist->form-urlencoded query))

(define (http-request-line-encode method path #:query (query #f))
  (string-append (http-encode-method method)
                 " "
                 (http-url-encode-path path)
                 (if query
                     (string-append "?" (http-url-encode-query query) " ")
                     " ")
                 (encode-protocol-version protocol-version)))

(define (http-request-write request (out (current-output-port)))
  (write-string
   (http-request-line-encode
    (http-request-method request)
    (http-request-path   request)
    #:query (http-request-query request))
   out)
  (write-string crlf out)
  (http-headers-write (http-request-headers request) out)
  (write-string crlf out)
  (let ((body (http-request-body request)))
    (when body (copy-port body out))))

(module+ test
  (test-case "http-url-encode-method"
    (check-equal? (http-encode-method 'get) "GET"))

  (test-case "http-url-encode-path"
    (check-equal? (http-url-encode-path "/foo/bar") "/foo/bar")
    (check-equal? (http-url-encode-path "/foo/bar baz") "/foo/bar%20baz"))

  (test-case "http-url-encode-query"
    (check-equal? (http-url-encode-query '((lain . "iwakura") (misami . "eiri")))
                  "lain=iwakura&misami=eiri")
    (check-equal? (http-url-encode-query '((action . "all love lain")))
                  "action=all+love+lain"))

  (test-case "http-request-line-encode"
    (check-equal? (http-request-line-encode 'get  "/foo/bar") "GET /foo/bar HTTP/1.1")
    (check-equal? (http-request-line-encode 'put  "/foo bar") "PUT /foo%20bar HTTP/1.1")
    (check-equal? (http-request-line-encode 'post "/foo bar") "POST /foo%20bar HTTP/1.1")
    (check-equal? (http-request-line-encode 'get  "/foo bar"
                                            #:query '((baz . "qux") (quax . "and yellow ducks")))
                  "GET /foo%20bar?baz=qux&quax=and+yellow+ducks HTTP/1.1"))

  (test-case "http-request-write"
    (check-equal?
     (let ((out (open-output-string)))
       (http-request-write (make-http-request "127.0.0.1" 8080 "/hello you"
                                              #:query '((foo . "bar"))
                                              #:headers '((Host . "example.com")))
                           out)
       (get-output-string out))
     (string-join (list "GET /hello%20you?foo=bar HTTP/1.1"
                        "Host: example.com"
                        "User-Agent: corpix-http/1.0"
                        "Accept: */*"
                        "Connection: close"
                        "" "")
                  "\r\n"))
    (check-equal?
     (let ((out (open-output-string)))
       (http-request-write (make-http-request "127.0.0.1" 8080 "/hello you"
                                              #:form '((foo . "bar"))
                                              #:headers '((Host . "example.com")))
                           out)
       (get-output-string out))
     (string-join (list "GET /hello%20you HTTP/1.1"
                        "Host: example.com"
                        "User-Agent: corpix-http/1.0"
                        "Accept: */*"
                        "Connection: close"
                        "Content-Type: application/x-www-form-urlencoded"
                        "Content-Length: 7"
                        ""
                        "foo=bar")
                  "\r\n"))
    (check-equal?
     (let ((out (open-output-string)))
       (parameterize ((current-multipart-form-random-source make-bytes))
         (http-request-write (make-http-request "127.0.0.1" 8080 "/hello you"
                                                #:multipart '((foo . "bar"))
                                                #:headers '((Host . "example.com")))
                             out))
       (get-output-string out))
     (string-join (list "GET /hello%20you HTTP/1.1"
                        "Host: example.com"
                        "User-Agent: corpix-http/1.0"
                        "Accept: */*"
                        "Connection: close"
                        "Content-Type: multipart/form-data; boundary=000000000000000000000000000000000000000000000000000000000000"
                        "Content-Length: 179"
                        ""
                        "--000000000000000000000000000000000000000000000000000000000000"
                        "Content-Disposition: form-data; name=foo"
                        ""
                        "bar"
                        "--000000000000000000000000000000000000000000000000000000000000--"
                        "")
                  "\r\n"))))

;;

(define (http-decode-status-line status)
  (let ((parts (string-split status " " #:trim? #f)))
    (values
     (decode-protocol-version (list-ref parts 0)) ;; protocol-version
     (string->number (list-ref parts 1)) ;; status
     (string-join (drop parts 2) " ")))) ;; status-text

(define-struct http-response
  (protocol-version status status-text headers body-reader)
  #:transparent
  #:mutable
  #:constructor-name -make-http-response)

(define/contract (make-http-response protocol-version status status-text headers body-reader)
  (-> (listof exact-nonnegative-integer?) integer? string?
      http-headers?
      input-port?
      http-response?)
  (-make-http-response protocol-version status status-text headers body-reader))

(define (http-response-read! (in (current-input-port)))
  (let-values (((protocol-version status status-text)
                (http-decode-status-line (read-until-crlf in))))
    (make-http-response protocol-version status status-text
                        (http-headers-read in)
                        in)))

(define (make-http-response-port client connection request response)
  (let* ((headers (http-response-headers response))
         (keep-alive? (http-headers-has (http-request-headers request)
                                        '(Connection . "keep-alive")))
         (body-reader (http-response-body-reader response))
         (content-length (string->number (http-header-ref headers 'Content-Length "")))
         (free (lambda (port)
                 (if keep-alive?
                     (http-connection-put! connection #:pool (http-client-pool client))
                     (close-input-port port)))))
    (cond ((exact-nonnegative-integer? content-length)
           (make-limited-input-port body-reader content-length #:on-close free))
          ((http-headers-has headers '(Transfer-Encoding . "chunked"))
           (make-chunked-input-port body-reader #:on-close free))
          (else body-reader))))

(define (http-request-send! req
                            #:client (client (current-http-client))
                            #:transport (transport (current-http-transport))
                            #:hijacker (hijacker #f))
  (let* ((connection (http-connection-get!
                      (http-request-host req)
                      (http-request-port req)
                      #:pool (http-client-pool client)
                      #:transport transport)))
    (let ((out (http-connection-out connection)))
      (http-request-write req out)
      (flush-output out))
    ;; TODO: support read timeout (for non hijacked requests?)
    (let ((res (http-response-read! (http-connection-in connection))))
      (when (not (eq? (version-compare (http-response-protocol-version res)
                                       protocol-version
                                       #:parts 'major)
                      '=))
        (error (format "want ~a version, got ~a"
                       protocol-version
                       (http-response-protocol-version res))))
      (if hijacker
          (hijacker connection res)
          (begin0 res
            (set-http-response-body-reader! res (make-http-response-port client connection req res)))))))

(module+ test
  (test-case "decode-status-line"
    (let-values (((protocol-version status status-text)
                  (http-decode-status-line "HTTP/1.1 200 OK")))
      (check-equal? (list protocol-version status status-text)
                    (list '(1 1) 200 "OK"))))

  (test-case "response-read"
    (let* ((in (open-input-string "HTTP/1.1 200 This is fine\r\nFoo: bar\r\n\r\nhello"))
           (response (http-response-read! in)))
      (check-equal? response
                    (make-http-response '(1 1) 200 "This is fine"
                                        (make-http-headers '("Foo" . "bar")) in))
      (check-equal? (port->string in) "hello"))))

;;

(define (http url
              #:client (client (current-http-client))
              #:method (method 'get)
              #:query (query #f)
              #:headers (headers #f)
              #:form (form #f)
              #:multipart (multipart #f)
              #:body (body #f)
              #:hijacker (hijacker #f))
  (let* ((u (if (url? url) url (string->url url)))
         (host (url-host u))
         (port (or (url-port u) (url-scheme->port (url-scheme u))))
         (query (or query (and (url-query? u) (form-urlencoded->alist (url-query u)))))
         (req (make-http-request host port
                                 (if (url-path? u) (url-path u) "/")
                                 #:method method
                                 #:query query
                                 #:headers headers
                                 #:form form
                                 #:multipart multipart
                                 #:body body)))
    (http-request-send! req
                        #:client client
                        ;; FIXME: cache transports, they have no state
                        #:transport (make-http-transport (url-scheme u))
                        #:hijacker hijacker)))

(define-syntax (with-request stx)
  (syntax-case stx ()
    ((_ (response-sym request-expr) body ...)
     (syntax/loc stx
       (let ((response-sym request-expr))
         (call-with-continuation-barrier
          (thunk (dynamic-wind
                   void
                   (thunk body ...)
                   (thunk (close-input-port (http-response-body-reader response-sym)))))))))))

;; (http "https://corpix.dev")

;; (define (http-get))
;; (define (http-head))
;; (define (http-post))
;; (define (http-put))
;; (define (http-delete))
;; (define (http-options))
