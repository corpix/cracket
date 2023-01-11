#lang racket
(require racket
         net/uri-codec
         openssl
         corpix/hex
         (only-in srfi/13 string-index)
         (only-in racket/port port->string)
         (for-syntax racket syntax/parse))
(provide transport<%>
         plaintext%
         tls%

         current-transport
         current-client
         current-user-agent
         current-pool

         (except-out (struct-out client) -make-client)
         (except-out (struct-out connection) -make-connection)
         (except-out (struct-out pool) -make-pool)
         (except-out (struct-out request) -make-request)
         (except-out (struct-out response) -make-response)

         make-client
         make-pool
         make-limited-input-port
         make-chunked-input-port
         make-headers
         make-default-headers
         make-response-port
         make-request
         make-response

         pool-put!
         pool-get!
         for/pool

         connection-get!
         connection-put!
         connect

         header-find
         header-ref
         header-append
         header-replace
         header-remove
         headers?
         headers-has
         headers-merge
         headers-write
         headers-read
         header-encode
         header-decode


         encode-method
         encode-path
         encode-query
         request-line-encode
         request-write
         decode-status-line

         response-read
         request-send!)
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

(define (make-limited-input-port port limit
                                 #:on-close (on-close close-input-port))
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

(define-struct pool (store semaphore)
  #:prefab
  #:constructor-name -make-pool)
(define (make-pool . values)
  (-make-pool (make-hash values)
              (make-semaphore 1)))
(define current-pool (make-parameter (make-pool)))

(define (pool-get! pool key (default #f))
  (define store (pool-store pool))
  (call-with-semaphore
   (pool-semaphore pool)
   (thunk
    (let ((bucket (hash-ref store key #f)))
      (if (and bucket (not (null? bucket)))
          (begin
            (hash-set! store key (cdr bucket))
            (car bucket))
          default)))))

(define (pool-put! pool key connection)
  (define store (pool-store pool))
  (call-with-semaphore
   (pool-semaphore pool)
   (thunk
    (hash-set! store key
               (cons connection
                     (hash-ref store key null))))))

;; FIXME: eliminate syntax template duplication
(define-syntax (for/pool stx)
  (syntax-case stx ()
    ((_ (((k v) pool)) body0 body ...)
     #'(let* ((p pool)
              (s (pool-store p)))
         (call-with-semaphore
          (pool-semaphore p)
          (thunk
           (for ((k (in-list (hash-keys s))))
             (for ((v (in-list (hash-ref s k))))
               body0 body ...))))))
    ((_ ((v pool)) body0 body ...)
     #'(let* ((p pool)
              (s (pool-store p)))
         (call-with-semaphore
          (pool-semaphore p)
          (thunk
           (for ((k (in-list (hash-keys s))))
             (for ((v (in-list (hash-ref s k))))
               body0 body ...))))))))

(module+ test
  (test-case "pool-get!"
    (let ((pool (make-pool)))
      (check-equal? (pool-get! pool 1) #f)
      (check-equal? (pool-get! pool 1 #f) #f))
    (let ((pool (make-pool)))
      (pool-put! pool 1 #t)
      (check-equal? (pool-get! pool 1) #t)
      (check-equal? (pool-get! pool 1 #f) #f)))

  (test-case "pool-put!"
    (let ((pool (make-pool)))
      (pool-put! pool 1 2)
      (check-equal? (pool-store pool)
                    (make-hash '((1 2))))
      (pool-put! pool 3 4)
      (check-equal? (pool-store pool)
                    (make-hash '((1 2) (3 4))))
      (pool-put! pool 1 9)
      (check-equal? (pool-store pool)
                    (make-hash '((1 9 2) (3 4))))
      (pool-put! pool 1 10)
      (check-equal? (pool-store pool)
                    (make-hash '((1 10 9 2) (3 4))))))

  (test-case "for/pool"
    (let ((acc null))
      (for/pool ((v (make-pool)))
        (set! acc (cons v acc)))
      (check-equal? acc null))
    (let ((acc null)
          (pool (make-pool)))
      (for ((n (reverse '(1 2 3 4))))
        (pool-put! pool 'first n))
      (for ((n (reverse '(5 6 7 8))))
        (pool-put! pool 'second n))
      (for/pool ((v pool))
        (set! acc (cons v acc)))
      (check-equal? (sort acc >) '(8 7 6 5 4 3 2 1)))
    (let ((acc null)
          (pool (make-pool)))
      (for ((n (reverse '(1 2 3 4))))
        (pool-put! pool 'first n))
      (for ((n (reverse '(5 6 7 8))))
        (pool-put! pool 'second n))
      (for/pool (((k v) pool))
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

(define-struct connection
  (transport host port in out)
  #:prefab
  #:constructor-name -make-connection)

(define transport<%> (interface () get-name connect))

(define plaintext%
  (class* object% (transport<%>)
    (super-new)
    (define/public (get-name) 'plaintext)
    (define/public (connect host port)
      (tcp-connect host port))))

(define tls%
  (class* object% (transport<%>)
    (super-new)
    (define/public (get-name) 'tls)
    (define/public (connect host port)
      (ssl-connect host port 'secure))))

(define current-transport (make-parameter (new plaintext%)))

(define (connect host port
                 #:transport (transport (current-transport)))
  (let-values (((in out) (send transport connect host port)))
    (-make-connection transport
                      host port
                      in out)))

(define (make-connection-key host port
                             #:transport (transport (current-transport)))
  (format "~a-~a-~a"
          (send transport get-name)
          host port))

(define (connection-get! host port
                         #:pool (pool (current-pool))
                         #:transport (transport (current-transport)))
  (or (let* ((key (make-connection-key host port #:transport transport))
             (connection (pool-get! pool key)))
        (and connection
             (match (sync/timeout 0 (peek-bytes-evt 1 0 #f (connection-in connection)))
               ((? eof-object?) #f) ;; connection timeout or closed
               (_ connection))))
      (connect host port #:transport transport)))

(define (connection-put! connection #:pool (pool (current-pool)))
  (pool-put! pool
             (make-connection-key (connection-host connection)
                                  (connection-port connection)
                                  #:transport (connection-transport connection))
             connection))

(module+ test
  (define test-transport%
    (class* object% (transport<%>)
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
          (pool      (make-pool)))
      (check-true (connection? (connection-get! "example.com" 666
                                                #:pool pool
                                                #:transport transport))))
    (let* ((transport  (new test-transport%))
           (connection (connect "example.com" 666 #:transport transport))
           (key        (make-connection-key "example.com" 666
                                            #:transport transport))
           (pool       (make-pool)))
      (pool-put! pool key connection)
      (let ((connection-from-pool (connection-get! "example.com" 666
                                                   #:pool pool
                                                   #:transport transport)))
        (check-eq? (connection-transport connection-from-pool) transport)
        (check-eq? (connection-in connection-from-pool) (connection-in connection))
        (check-eq? (connection-out connection-from-pool) (connection-out connection)))
      (let ((connection-from-pool (connection-get! "example.com" 666
                                                   #:pool pool
                                                   #:transport transport)))
        (check-not-eq? connection-from-pool connection)
        (check-eq? (connection-transport connection-from-pool) transport))))

  (test-case "connection-put!"
    (let* ((transport  (new test-transport%))
           (connection (connect "example.com" 666 #:transport transport))
           (pool       (make-pool)))
      (connection-put! connection
                       #:pool pool)
      (let ((connection-from-pool (connection-get! "example.com" 666
                                                   #:pool pool
                                                   #:transport transport)))
        (check-eq? (connection-transport connection-from-pool) transport)
        (check-eq? (connection-in connection-from-pool) (connection-in connection))
        (check-eq? (connection-out connection-from-pool) (connection-out connection)))
      (let ((connection-from-pool (connection-get! "example.com" 666
                                                   #:pool pool
                                                   #:transport transport)))
        (check-not-eq? connection connection-from-pool)
        (check-eq? (connection-transport connection-from-pool) transport)))))

;;

(define-struct client
  (pool)
  #:prefab
  #:constructor-name -make-client)

(define/contract (make-client (pool (current-pool)))
  (-> client?)
  (-make-client pool))

(define current-client (make-parameter (make-client)))

;;

(define current-user-agent (make-parameter "corpix-http/1.0"))

;; FIXME: this should not be a macro
(define-syntax (make-headers stx)
  (syntax-case stx ()
    ((_ (name value) ...)
     #'(list (cons name  value) ...))))

(define (make-default-headers host)
  (make-headers ('Host host)
                ('User-Agent (current-user-agent))
                ('Accept "*/*")
                ('Connection "close")))

(define (header-find headers name (default #f))
  (let ((name (if (symbol? name)
                  (symbol->string name)
                  name))
        (len (length headers)))
    (let loop ((i 0))
      (if (>= i len)
          default
          (let ((header (list-ref headers i)))
            (if (string-ci=? (symbol->string (car header)) name)
                (cons i header)
                (loop (add1 i))))))))

(define (header-ref headers name (default #f))
  (let ((record (header-find headers name)))
    (if record
        (cddr record)
        default)))

;; FIXME: this should not be a macro
(define-syntax (header-append stx)
  (syntax-case stx ()
    ((_ headers (name0 value0) (name value) ...)
     #'(for/fold ((hs headers))
                 ((n (list name0  name ...))
                  (v (list value0 value ...)))
         (append hs (list (cons n v)))))))

;; FIXME: this should not be a macro
(define-syntax (header-replace stx)
  (syntax-case stx ()
    ((_ headers (name0 value0) (name value) ...)
     #'(for/fold ((hs headers))
                 ((n (list name0  name ...))
                  (v (list value0 value ...)))
         (let ((record (header-find hs n)))
           (if record
               (let-values (((left right)
                             (split-at hs (car record))))
                 (append
                  left
                  (list (cons n v))
                  (drop right 1)))
               (append
                hs
                (list (cons n v)))))))))

;; FIXME: this should not be a macro
(define-syntax (header-remove stx)
  (syntax-case stx ()
    ((_ headers name0 name ...)
     #'(for/fold ((hs headers))
                 ((n (list name0 name ...)))
         (let ((record (header-find hs n)))
           (if record
               (let-values (((left right) (split-at hs (car record))))
                 (append left (drop right 1)))
               hs))))))

(define (headers? v)
  ((listof (cons/c symbol? string?)) v))

;; FIXME: this should not be a macro
(define-syntax (headers-has stx)
  (syntax-case stx ()
    ((_ headers (name value))
     #'(let ((v (header-ref headers name #f)))
         (and v (equal? v value))))))

(define (headers-merge original update)
  (for/fold ((hs original))
            ((h (in-list update)))
    (header-replace hs ((car h) (cdr h)))))

(define (headers-write headers (out (current-output-port)))
  (for ((header headers))
    (write-string (header-encode header) out)
    (write-string crlf out)))

(define (headers-read (in (current-input-port)))
  (let loop ((acc null))
    (define v (read-until-crlf in))
    (if (or (eof-object? v)
            (= (string-length v) 0))
        (reverse acc)
        (loop (cons (header-decode v)
                    acc)))))

(define (header-encode header)
  (string-append (symbol->string (car header))
                 ": "
                 (cdr header)))

(define (header-decode header)
  (let ((index (string-index header #\:)))
    (when (not index)
      (error "header delimiter was not found"))
    (cons
     (string->symbol (string-trim (substring header 0 index)))
     (string-trim (substring header (add1 index))))))

(module+ test
  (test-case "make-headers"
    (check-equal? (make-headers ('foo "bar") ('baz "qux"))
                  (list (cons 'foo "bar")
                        (cons 'baz "qux"))))

  (test-case "make-default-headers"
    (check-true ((listof (cons/c symbol? string?))
                 (make-default-headers "example.com")))
    (check-equal? (assoc 'Host (make-default-headers "example.com"))
                  '(Host . "example.com")))

  (test-case "header-find"
    (check-equal? (header-find (make-headers ('Host "example.com"))
                               'host)
                  '(0 Host . "example.com"))
    (check-equal? (header-find (make-headers ('Connection "keep-alive"))
                               'Connection)
                  '(0 Connection . "keep-alive"))
    (check-equal? (header-find (make-headers ('Host  "example.com")
                                             ('Connection "keep-alive"))
                               'Connection)
                  '(1 Connection . "keep-alive"))
    (check-equal? (header-find (make-headers ('Host "example.com")
                                             ('Connection "keep-alive")
                                             ('Connection "close"))
                               'Connection)
                  '(1 Connection . "keep-alive"))

    (check-equal? (header-find (make-headers ('Host "example.com")) 'f) #f)
    (check-equal? (header-find (make-headers) 'f) #f)
    (check-equal? (header-find (make-headers) 'f (void)) (void)))

  (test-case "header-ref"
    (check-equal? (header-ref (list '(Connection . "keep-alive")) 'Connection)
                  "keep-alive"))

  (test-case "header-append"
    (check-equal? (header-append (make-headers ('Connection "keep-alive"))
                                 ('Cache-Control "no-cache"))
                  (make-headers ('Connection "keep-alive")
                                ('Cache-Control "no-cache")))
    (check-equal? (header-append (make-headers ('Connection "keep-alive"))
                                 ('Cache-Control "no-cache")
                                 ('Content-Type "text/html"))
                  (make-headers ('Connection "keep-alive")
                                ('Cache-Control "no-cache")
                                ('Content-Type "text/html"))))

  (test-case "header-replace"
    (check-equal? (header-replace (make-headers ('Connection "keep-alive"))
                                  ('Connection "close"))
                  (make-headers ('Connection "close")))
    (check-equal? (header-replace (make-headers)
                                  ('Host "example.com"))
                  (make-headers ('Host "example.com")))
    (check-equal? (header-replace (list)
                                  ('Connection "close")
                                  ('Connection "keep-alive"))
                  (list '(Connection . "keep-alive"))))

  (test-case "header-remove"
    (check-equal? (header-remove (make-headers ('Connection "keep-alive"))
                                 'Connection)
                  (make-headers))
    (check-equal? (header-remove (make-headers ('Connection "keep-alive")
                                               ('Connection "close"))
                                 'Connection)
                  (make-headers ('Connection "close"))) ;; XXX: shifts one item at a time
    (check-equal? (header-remove (make-headers ('Connection "keep-alive")
                                               ('Cache-Control "no-cache"))
                                 'Connection)
                  (make-headers ('Cache-Control "no-cache")))
    (check-equal? (header-remove (make-headers ('Connection "keep-alive")
                                               ('Cache-Control "no-cache"))
                                 'Connection
                                 'Cache-Control)
                  (make-headers)))

  (test-case "headers-has"
    (check-equal? (headers-has (make-headers ('Foo "bar")) ('Foo "bar"))
                  #t)
    (check-equal? (headers-has (make-headers ('Foo "bar")) ('Bar "baz"))
                  #f))

  (test-case "headers-merge"
    (check-equal? (headers-merge '()
                                 '((Hello . "me") (Bar . "baz")))
                  '((Hello . "me") (Bar . "baz")))
    (check-equal? (headers-merge '((Hello . "you") (Foo . "bar"))
                                 '((Hello . "me") (Bar . "baz")))
                  '((Hello . "me") (Foo . "bar") (Bar . "baz")))
    (check-equal? (headers-merge '((hello . "you"))
                                 '((Hello . "me") (Bar . "baz")))
                  '((Hello . "me") (Bar . "baz"))))

  (test-case "headers-write"
    (let ((out (open-output-string)))
      (check-equal? (begin
                      (headers-write '((Host . "localhost")) out)
                      (get-output-string out))
                    "Host: localhost\r\n"))
    (let ((out (open-output-string)))
      (check-equal? (begin
                      (headers-write '((Host . "localhost")
                                       (User-Agent . "test"))
                                     out)
                      (get-output-string out))
                    "Host: localhost\r\nUser-Agent: test\r\n"))

    (let ((out (open-output-string)))
      (check-equal? (begin
                      (headers-write '((Set-Cookie . "name1=value; HttpOnly")
                                       (Set-Cookie . "name2=value; HttpOnly"))
                                     out)
                      (get-output-string out))
                    "Set-Cookie: name1=value; HttpOnly\r\nSet-Cookie: name2=value; HttpOnly\r\n")))

  (test-case "headers-read"
    (let ((in (open-input-string "Foo: bar\r\nBaz: qux")))
      (check-equal? (headers-read in)
                    (list '(Foo . "bar") '(Baz . "qux"))))
    (let ((in (open-input-string "Foo: bar\r\nBaz: qux\r\n\r\n")))
      (check-equal? (headers-read in)
                    (list '(Foo . "bar") '(Baz . "qux")))))

  (test-case "header-decode"
    (check-equal? (header-decode "Foo: bar")
                  '(Foo . "bar"))
    (check-equal? (header-decode "Foo:bar")
                  '(Foo . "bar"))
    (check-equal? (header-decode "Foo  :  bar")
                  '(Foo . "bar"))))

;;

(define-struct request
  (host port method path query headers body)
  #:prefab
  #:constructor-name -make-request)

(define/contract (make-request host port path
                              #:method (method 'get)
                              #:query (query #f)
                              #:headers (headers #f)
                              #:body (body #f))
  (->* (string? exact-nonnegative-integer? string?)
       (#:method symbol?
        #:query (listof (cons/c symbol? (or/c false/c string?)))
        #:headers (listof (or/c false/c (cons/c symbol? string?)))
        #:body (or/c false/c string? bytes? input-port?))
       request?)
  (let* ((headers (if (not headers)
                      (make-default-headers host)
                      (headers-merge (make-default-headers host) headers)))
         (body (and body
                    (match (header-ref headers 'Content-Type)
                      ("application/x-www-form-urlencoded"
                       (let ((data
                              (cond
                                ((input-port? body) (port->bytes body))
                                ((string? body) (string->bytes/utf-8 body))
                                ((bytes? body) body))))
                         (set! headers (header-replace
                                        headers
                                        ('Content-Length (number->string (bytes-length data)))))
                         (open-input-bytes data)))))))
    (-make-request host port method path query headers body)))

(define (encode-method method)
  (string-upcase (symbol->string method)))

(define (encode-path path)
  (string-join
   (map uri-encode
        (string-split path "/" #:trim? #f))
   "/"))

(define (encode-query query)
  (alist->form-urlencoded query))

(define (request-line-encode method path #:query (query #f))
  (string-append (encode-method method)
                 " "
                 (encode-path path)
                 (if query
                     (string-append "?" (encode-query query) " ")
                     " ")
                 (encode-protocol-version protocol-version)))

(define (request-write request (out (current-output-port)))
  (write-string
   (request-line-encode
    (request-method request)
    (request-path   request)
    #:query (request-query request))
   out)
  (write-string crlf out)
  (headers-write (request-headers request) out)
  (write-string crlf out)
  (let ((body (request-body request)))
    (when body (copy-port body out))))

(module+ test
  (test-case "encode-method"
    (check-equal? (encode-method 'get) "GET"))

  (test-case "encode-path"
    (check-equal? (encode-path "/foo/bar") "/foo/bar")
    (check-equal? (encode-path "/foo/bar baz") "/foo/bar%20baz"))

  (test-case "encode-query"
    (check-equal? (encode-query '((lain . "iwakura") (misami . "eiri")))
                  "lain=iwakura&misami=eiri")
    (check-equal? (encode-query '((action . "all love lain")))
                  "action=all+love+lain"))

  (test-case "request-line-encode"
    (check-equal? (request-line-encode 'get  "/foo/bar") "GET /foo/bar HTTP/1.1")
    (check-equal? (request-line-encode 'put  "/foo bar") "PUT /foo%20bar HTTP/1.1")
    (check-equal? (request-line-encode 'post "/foo bar") "POST /foo%20bar HTTP/1.1")
    (check-equal? (request-line-encode 'get  "/foo bar"
                                       #:query '((baz . "qux") (quax . "and yellow ducks")))
                  "GET /foo%20bar?baz=qux&quax=and+yellow+ducks HTTP/1.1"))

  (test-case "request-write"
    (let ((out (open-output-string)))
      (check-equal?
       (begin
         (request-write (make-request "127.0.0.1" 8080 "/hello you"
                                     #:query '((foo . "bar"))
                                     #:headers '((Host . "example.com")))
                        out)
         (get-output-string out))
       "GET /hello%20you?foo=bar HTTP/1.1\r\nHost: example.com\r\nUser-Agent: corpix-http/1.0\r\nAccept: */*\r\nConnection: close\r\n\r\n"))))

;;

(define (decode-status-line status)
  (let ((parts (string-split status " " #:trim? #f)))
    (values
     (decode-protocol-version (list-ref parts 0)) ;; protocol-version
     (string->number (list-ref parts 1))             ;; status
     (string-join (drop parts 2) " "))))             ;; status-text

(define-struct response
  (protocol-version status status-text headers body-reader)
  #:prefab
  #:constructor-name -make-response)

(define/contract (make-response protocol-version status status-text headers body-reader)
  (-> (listof exact-nonnegative-integer?) integer? string?
      headers?
      input-port?
      response?)
  (-make-response protocol-version status status-text headers body-reader))

(define (response-read (in (current-input-port)))
  (let-values (((protocol-version status status-text)
                (decode-status-line (read-until-crlf in))))
    (make-response protocol-version status status-text
                   (headers-read in)
                   in)))

(define (make-response-port client connection request response)
  (let* ((headers (response-headers response))
         (keep-alive? (headers-has (request-headers request) ('Connection "keep-alive")))
         (body-reader (response-body-reader response))
         (content-length (string->number (header-ref headers 'Content-Length "")))
         (free (lambda (port)
                 (if keep-alive?
                     (connection-put! connection
                                      #:pool (client-pool client))
                     (close-input-port port)))))
    (cond ((exact-nonnegative-integer? content-length)
           (make-limited-input-port body-reader content-length #:on-close free))
          ((headers-has headers ('Transfer-Encoding "chunked"))
           (make-chunked-input-port body-reader #:on-close free))
          (else body-reader))))

(define (request-send! req
                       #:client    (client    (current-client))
                       #:transport (transport (current-transport)))
  (let* ((connection (connection-get! (request-host req)
                                      (request-port req)
                                      #:pool (client-pool client)
                                      #:transport transport)))
    (let ((out (connection-out connection)))
      (request-write req out)
      (flush-output out))
    (let ((res (response-read (connection-in connection))))
      (when (not (eq?
                  (version-compare (response-protocol-version res)
                                   protocol-version
                                   #:parts 'major)
                  '=))
        (error (format "want ~a version, got ~a"
                       protocol-version
                       (response-protocol-version res))))
      (struct-copy
       response res
       (body-reader (make-response-port client connection req res))))))

(module+ test
  (test-case "decode-status-line"
    (let-values (((protocol-version status status-text)
                  (decode-status-line "HTTP/1.1 200 OK")))
      (check-equal? (list protocol-version status status-text)
                    (list '(1 1) 200 "OK"))))

  (test-case "response-read"
    (let* ((in (open-input-string "HTTP/1.1 200 This is fine\r\nFoo: bar\r\n\r\nhello"))
           (response (response-read in)))
      (check-equal? response
                    (make-response '(1 1) 200 "This is fine"
                                   (make-headers ('Foo "bar")) in))
      (check-equal? (port->string in) "hello"))))
