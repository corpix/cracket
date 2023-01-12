#lang racket
(require srfi/13
         corpix/hex)
(provide default-scheme-port-dispatcher
         current-url-scheme-port-dispatcher

         (struct-out url)
         make-url

         url-encode?
         url-encoded?
         url-encode
         url-decode

         url-trim-anchor

         url-consume-scheme
         url-consume-user
         url-consume-host
         url-consume-path
         url-consume-query

         url-produce-scheme
         url-produce-user
         url-produce-host
         url-produce-path
         url-produce-query

         string->url
         url->string

         url-scheme->port
         url->path
         url-http?
         url-https?)

;; inspired by Go url parser
;; see: https://golang.org/src/net/url/url.go

(define-struct url
  (
   scheme? scheme
   user? user
   host? host
   port? port
   path? path
   query? query)
  #:constructor-name -make-url
  #:transparent)

(define (make-url
         #:scheme (scheme #f)
         #:user   (user   #f)
         #:host   (host   #f)
         #:port   (port   #f)
         #:path   (path   #f)
         #:query  (query  #f))
  (-make-url
   (and scheme #t) (or scheme "")
   (and user   #t) (or user (list))
   (and host   #t) (or host "")
   (and port   #t) (or port #f)
   (and path   #t) (or path "")
   (and query  #t) (or query "")))

;;

(define default-scheme-port-dispatcher
  (let* ((t (make-hash))
         (put! (lambda (k v)
                 (begin0 t
                   (hash-set! t k v)))))
    (put! "http"  80)
    (put! "https" 443)))

(define current-url-scheme-port-dispatcher
  (make-parameter default-scheme-port-dispatcher))

;;

(define url-encode-modes
  '(path path-segment host zone user query-component fragment))

;; https://tools.ietf.org/html/rfc3986
(define (url-encode? n (mode #f))
  ;; see: https://golang.org/src/net/url/url.go?#L100
  (cond
    ((or (and (>= n #x61) (<= n #x7a))                       ;; a-z
         (and (>= n #x30) (<= n #x39))                       ;; 0-9
         (and (>= n #x41) (<= n #x5a)))                      ;; A-Z
     #f)                                                     ;;
    ((and (or (eq? 'fragment mode) (eq? 'path mode))         ;;
          (or (= #x21 n)                                     ;; !
              (= #x22 n)                                     ;; "
              (= #x24 n)                                     ;; $
              (and (>= n #x26) (<= n #x2c))                  ;; &'()*+,
              (= #x3a n)                                     ;; :
              (= #x3b n)                                     ;; ;
              (= #x3d n)                                     ;; =
              (= #x40 n)                                     ;; @
              (= #x5b n)                                     ;; [
              (= #x5d n)))                                   ;; ]
     #f)                                                     ;;
    ((and (or (eq? 'host mode) (eq? 'zone mode))             ;;
          (or (= #x21 n)                                     ;; !
              (= #x22 n)                                     ;; "
              (= #x24 n)                                     ;; $
              (and (>= n #x26) (<= n #x2c))                  ;; &'()*+,
              (and (>= n #x3a) (<= n #x3d))                  ;; :;<>=
              (= #x5b n)                                     ;; [
              (= #x5d n)))                                   ;; ]
     #f)                                                     ;;
    ((or (= #x2d n)                                          ;; -
         (= #x2e n)                                          ;; .
         (= #x5f n)                                          ;; _
         (= #x7e n))                                         ;; ~
     #f)                                                     ;;
    ((or (= #x24 n)                                          ;; $
         (= #x26 n)                                          ;; &
         (= #x2b n)                                          ;; +
         (= #x2c n)                                          ;; ,
         (= #x2f n)                                          ;; /
         (= #x3a n)                                          ;; :
         (= #x3b n)                                          ;; ;
         (= #x3d n)                                          ;; =
         (= #x3f n)                                          ;; ?
         (= #x40 n))                                         ;; @
     (cond                                                   ;;
       ((eq? 'path mode)                                     ;;
        (= #x3f n))                                          ;; ?
       ((eq? 'path-segment mode)                             ;;
        (or (= #x2c n)                                       ;; ,
            (= #x2f n)                                       ;; /
            (= #x3b n)                                       ;; ;
            (= #x3f n)))                                     ;; ?
       ((eq? 'user mode)                                     ;;
        (or (= #x2f n)                                       ;; /
            (= #x3a n)                                       ;; :
            (= #x3f n)                                       ;; ?
            (= #x40 n)))                                     ;; @
       ((eq? 'query-component mode) #t)                      ;;
       ((eq? 'fragment mode) #f)                             ;;
       (else #t)))                                           ;;
    (else #t)))

(define (url-encoded? u (mode #f))
  (let loop ((l (string-length u)) (n 0))
    (if (>= n l) #t
        (let ((c (char->integer (string-ref u n))))
          (and (or (= #x25 c) (not (url-encode? c mode)))
               (loop l (+ 1 n)))))))

(define (url-encode u (mode #f))
  (define len (string-length u))
  (define-values (space-count hex-count)
    (let loop ((n 0) (space 0) (hex 0))
      (if (= n len)
          (values space hex)
          (let ((c (char->integer (string-ref u n))))
            (if (url-encode? c mode)
                (if (and (= #x20 c) (eq? 'query-component mode)) ;; space
                    (loop (+ 1 n) (+ 1 space) hex)
                    (loop (+ 1 n) space (+ 1 hex)))
                (loop (+ 1 n) space hex))))))
  (if (and (= 0 space-count) (= 0 hex-count)) u
      (bytes->string/utf-8
       (let loop ((buf (make-bytes (+ len (* 2 hex-count))))
                  (n 0) (i 0))
         (if (= n len) buf
             (let ((c (char->integer (string-ref u n))))
               (cond ((and (= #x20 c) (eq? 'query-component mode)) ;; space
                      (bytes-set! buf i #x2b) ;; +
                      (loop buf (+ 1 n) (+ 1 i)))
                     ((url-encode? c mode)
                      (bytes-set! buf i #x25) ;; %
                      (bytes-set! buf (+ 1 i) (char->integer (integer->hex-char (arithmetic-shift c -4))))
                      (bytes-set! buf (+ 2 i) (char->integer (integer->hex-char (bitwise-and c 15))))
                      (loop buf (+ 1 n) (+ 3 i)))
                     (else
                      (bytes-set! buf i c)
                      (loop buf (+ 1 n) (+ 1 i))))))))))

(define (url-decode u (mode #f))
  (define len (string-length u))
  (define-values (i plus?)
    (let loop ((n 0) (i 0) (plus? #f))
      (if (>= n len)
          (values i plus?)
          (let ((c (char->integer (string-ref u n))))
            (cond
              ((= #x25 c)                        ;; %
               (when (or (>=  (+ 2 n) len) ;; incomplete hex byte
                         (not (hex-char? (string-ref u (+ 1 n))))
                         (not (hex-char? (string-ref u (+ 2 n)))))
                 (error "encode error at:"
                        (substring u (if (> (+ 3 n) len) (+ 3 n) n) len)))
               (case mode
                 ((host)
                  ;; https://tools.ietf.org/html/rfc3986#page-21
                  ;; https://tools.ietf.org/html/rfc6874#section-2
                  ;; first says %-encoding can only be used for non-ASCII bytes
                  ;; second says %25 could be used to encode percent sign in IPv6 addr
                  (when (not (equal? "%25" (substring u n (+ 3 n))))
                    (error "encode error at:" (substring u n len))))
                 ((zone)
                  (let ((nibble (bitwise-ior
                                 (arithmetic-shift (hex-char->integer (string-ref u (+ 1 n))) 4)
                                 (hex-char->integer (string-ref u (+ 2 n))))))
                    (when (and (not (equal? "%25" (substring u n (+ 3 n))))
                               (= #x20 nibble)
                               (url-encode? nibble 'host))
                      (error "encode error at:" (substring u n len)))))
                 (else (loop (+ 3 n) (+ 1 i) plus?))))
              ((= #x2b c) ;; +
               (loop (+ 1 n) i (eq? 'query-component mode)))
              ((and (or (eq? 'host mode)
                        (eq? 'zone mode))
                    (> #x80 c)
                    (url-encode? c mode))
               (error "invalid host error at:" c (url-encode? c mode) (> #x80 c) (substring u n len)))
              (else (loop (+ 1 n) i plus?)))))))
  (if (and (eq? 0 i) (not plus?)) u
      (bytes->string/utf-8
       (let loop ((buf (make-bytes (- (string-length u) (* 2 i))))
                  (n 0) (i 0))
         (if (= n len) buf
             (let ((c (char->integer (string-ref u n))))
               (cond
                 ((eq? #x25 c) ;; %
                  (bytes-set! buf i (bitwise-ior
                                     (arithmetic-shift (hex-char->integer (string-ref u (+ 1 n))) 4)
                                     (hex-char->integer (string-ref u (+ 2 n)))))
                  (loop buf (+ 3 n) (+ 1 i)))
                 ((eq? #x2b c) ;; +
                  (bytes-set! buf i (if (eq? 'query-component mode) #x20 #x2b)) ;; space +
                  (loop buf (+ 1 n) (+ 1 i)))
                 (else
                  (bytes-set! buf i c)
                  (loop buf (+ 1 n) (+ 1 i))))))))))

;;

(define (url-trim-anchor u)
  (if (= 0 (string-length u))
      u (car (string-split u "#"))))

;;

(define (url-consume-scheme u)
  (define len (string-length u))
  (let loop ((n 0))
    (if (= n len)
        (values #f "" u)
        (let ((c (char->integer (string-ref u n))))
          (cond ((or (and (>= c #x61) (<= c #x7a))  ;; a-z
                     (and (>= c #x41) (<= c #x5a))) ;; A-Z
                 (loop (+ 1 n)))
                ((or (and (>= c #x30) (<= c #x39))     ;; 0-9
                     (= #x2b c) (= #x2d c) (= #x2e c)) ;; +-.
                 (if (= 0 n)
                     (values #f "" u)
                     (loop (+ 1 n))))
                ((= #x3a c) ;; :
                 (let ((rest (substring u (+ 1 n) len)))
                   (cond ((string-prefix? "//" rest) ;; drop // prefix
                          (values (> n 0)
                                  (substring u 0 n)
                                  (substring rest 2)))
                         (else (values #f "" u)))))
                ;; invalid character, no valid scheme
                (else (values #f "" u)))))))

(define (url-consume-user u)
  (let* ((delim (string-index-right
                 (substring
                  u 0 (or (string-index u #\/)
                          (string-length u)))
                 #\@)))
    (if delim
        (let* ((user (substring u 0 delim))
               (len  (string-length user)))
          (let loop ((n 0))
            (if (= n len)
                (let ((info (string-split user #\:)))
                  (values #t
                          (cons (url-decode (car info) 'user)
                                (and (pair? (cdr info))
                                     (url-decode (cadr info) 'user)))
                          (substring u
                                     (+ 1 delim)
                                     (string-length u))))
                (let ((c (char->integer (string-ref user n))))
                  (if (or (and (>= c #x61) (<= c #x7a)) ;; a-z
                          (and (>= c #x41) (<= c #x5a)) ;; A-Z
                          (and (>= c #x30) (<= c #x39)) ;; 0-9
                          (and (>= c #x24) (<= c #x2e)) ;; $%&'()*+,-.
                          (= #x21 c)                    ;; !
                          (= #x3a c)                    ;; :
                          (= #x3b c)                    ;; ;
                          (= #x3d c)                    ;; =
                          (= #x40 c)                    ;; @
                          (= #x5f c)                    ;; _
                          (= #x7e c))                   ;; ~
                      (loop (+ 1 n))
                      (values #f (list) u))))))
        (values #f (list) u))))

(define (url-consume-host u)
  (let ((slash (string-index u #\/)))
    (if (and slash (= 0 slash))
        (values #f "" u)
        (let* ((end (or slash (string-length u)))
               (uu (substring u 0 end)))
          (let ((open-bracket (string-index uu #\[)) ;; ipv6
                (close-bracket (string-index-right uu #\])))
            (if (and open-bracket (= 0 open-bracket) close-bracket)
                ;; rfc 6874 defines that %25 (%-encoded percent) introduces
                ;; the zone identifier, and the zone identifier can use basically
                ;; any %-encoding it likes. that's different from the host, which
                ;; can only %-encode non-ascii bytes
                (let* ((address (substring uu open-bracket (+ 1 close-bracket)))
                       (zone (string-contains address "%25")))
                  (values
                   #t
                   (if zone
                       (string-append/shared
                        (url-decode (substring address 0 zone) 'host)
                        (url-decode (substring address zone (+ 1 close-bracket)) 'zone))
                       (url-decode address 'host))
                   (substring u (+ 1 close-bracket) (string-length u))))
                (let ((colon (string-index uu #\:)))
                  (values
                   #t
                   (url-decode
                    (if colon (substring uu 0 colon) uu)
                    'host)
                   (substring u (or colon end))))))))))

(define (url-consume-port u)
  (let ((colon (string-index u #\:)))
    (if (and colon (= 0 colon))
        (let* ((end (or (string-index u #\/)
                        (string-length u)))
               (port (substring u 1 end))
               (ok (let loop ((n 0) (l (string-length port)))
                     (if (>= n l)
                         (> l 0)
                         (let ((c (char->integer (string-ref port n))))
                           (and (<= #x30 c) (>= #x39 c) ;; 0-9
                                (loop (+ 1 n) l)))))))
          (if ok
              (values
               #t
               (string->number port)
               (substring u end (string-length u)))
              (values #f #f u)))
        (values #f #f u))))

(define (url-consume-path u)
  (let ((query (or (string-index u #\?) (string-length u)))
        (slash (string-index u #\/)))
    (if (and slash (= 0 slash))
        (values #t
                (url-decode (substring u slash query) 'path)
                (substring u query))
        (values #f "" u))))

(define (url-consume-query u)
  (let ((query (string-index u #\?)))
    (cond
      ((and query (= 0 query))
       (values (> (string-length u) 1)
               (substring u 1)
               ""))
      (else (values #f "" "")))))

(define (string->url s)
  (let*-values (((scheme? scheme rest) (url-consume-scheme (url-trim-anchor s)))
                ((user?   user   rest) (url-consume-user rest))
                ((host?   host   rest) (url-consume-host rest))
                ((port?   port   rest) (url-consume-port rest))
                ((path?   path   rest) (url-consume-path rest))
                ((query?  query  rest) (url-consume-query rest)))
    (-make-url scheme? scheme
               user?   user
               host?   host
               port?   port
               path?   path
               query?  query)))

;;

(define (url-produce-scheme u)
  (if (url-scheme? u)
      (string-append (url-scheme u) "://")
      ""))

(define (url-produce-user u)
  (if (url-user? u)
      (string-append (url-user u) "@")
      ""))

(define (url-produce-host u)
  (if (url-host? u)
      (url-encode (url-host u) 'host)
      ""))

(define (url-produce-port u)
  (if (url-port? u)
      (string-append ":" (number->string (url-port u)))
      ""))

(define (url-produce-path u)
  (if (url-path? u)
      (let ((path (url-path u)))
        (if (url-encoded? path 'path) path
            (url-encode path 'path)))
      ""))

(define (url-produce-query u)
  (if (url-query? u)
      (string-append "?" (url-query u))
      ""))

;;

(define (url->string u)
  (string-append/shared
   (url-produce-scheme u)
   (url-produce-user u)
   (url-produce-host u)
   (url-produce-port u)
   (url-produce-path u)
   (url-produce-query u)))

;;

(define (url-scheme->port scheme (default #f))
  (or (hash-ref (current-url-scheme-port-dispatcher) scheme default)
      (error (format "unknown port number for scheme ~s" scheme))))

(define (url->path u)
  (string-append
   (if (url-path? u) (url-produce-path u) "/")
   (url-produce-query u)))

(define (url-http? url)
  (equal? (url-scheme url) "http"))

(define (url-https? url)
  (equal? (url-scheme url) "https"))

(module+ test
  (require rackunit)
  (test-case "url-encode"
    (check-equal? (url-encode "?!@#$%")
                  "%3f%21%40%23%24%25"))
  (test-case "string->url"
    (check-equal? (string->url "http://localhost")
                  (make-url #:scheme "http" #:host "localhost"))
    (check-equal? (string->url "http://localhost/test")
                  (make-url #:scheme "http" #:host "localhost" #:path "/test"))
    (check-equal? (string->url "https://localhost")
                  (make-url #:scheme "https" #:host "localhost"))
    (check-equal? (string->url "https://localhost:44344")
                  (make-url #:scheme "https" #:host "localhost" #:port 44344))
    (check-equal? (string->url "https://localhost/test")
                  (make-url #:scheme "https" #:host "localhost" #:path "/test"))
    (check-equal? (string->url "https://localhost:5566/?foo=bar")
                  (make-url #:scheme "https" #:host "localhost" #:port 5566
                            #:path "/" #:query "foo=bar"))
    (check-equal? (string->url "https://localhost/test?foo=bar")
                  (make-url #:scheme "https" #:host "localhost"
                            #:path "/test" #:query "foo=bar"))
    (check-equal? (string->url "https://localhost:6655/test?foo=bar")
                  (make-url #:scheme "https" #:host "localhost" #:port 6655
                            #:path "/test" #:query "foo=bar"))
    (check-equal? (string->url "https://localhost/test?foo=bar&baz=qux")
                  (make-url #:scheme "https" #:host "localhost"
                            #:path "/test" #:query "foo=bar&baz=qux")))
  (test-case "url->string"
    (check-equal? (url->string (make-url #:scheme "http" #:host "localhost"))
                  "http://localhost")
    (check-equal? (url->string (make-url #:scheme "http" #:host "localhost" #:path "/test"))
                  "http://localhost/test")
    (check-equal? (url->string (make-url #:scheme "https" #:host "localhost"))
                  "https://localhost")
    (check-equal? (url->string (make-url #:scheme "https" #:host "localhost" #:port 44344))
                  "https://localhost:44344")
    (check-equal? (url->string (make-url #:scheme "https" #:host "localhost" #:path "/test"))
                  "https://localhost/test")
    (check-equal? (url->string (make-url #:scheme "https" #:host "localhost" #:port 5566
                                         #:path "/" #:query "foo=bar"))
                  "https://localhost:5566/?foo=bar")
    (check-equal? (url->string (make-url #:scheme "https" #:host "localhost"
                                         #:path "/test" #:query "foo=bar"))
                  "https://localhost/test?foo=bar")
    (check-equal? (url->string (make-url #:scheme "https" #:host "localhost" #:port 6655
                                         #:path "/test" #:query "foo=bar"))
                  "https://localhost:6655/test?foo=bar")
    (check-equal? (url->string (make-url #:scheme "https" #:host "localhost"
                                         #:path "/test" #:query "foo=bar&baz=qux"))
                  "https://localhost/test?foo=bar&baz=qux"))
  (test-case "url-scheme->port"
    (check-eqv? (url-scheme->port "http") 80)
    (check-eqv? (url-scheme->port "https") 443)
    (check-exn exn? (thunk (url-scheme->port "unknown")))
    (check-eqv? (url-scheme->port "unknown" 'default)
                'default)
    (parameterize ((current-url-scheme-port-dispatcher (make-hash)))
      (hash-set! (current-url-scheme-port-dispatcher) "some" 666)
      (check-eqv? (url-scheme->port "some") 666)))
  (test-case "url->path"
    (check-equal? (url->path (string->url "http://example.com"))
                  "/")
    (check-equal? (url->path (string->url "http://example.com/"))
                  "/")
    (check-equal? (url->path (string->url "http://example.com/test"))
                  "/test")
    (check-equal? (url->path (string->url "http://example.com/?foo=bar"))
                  "/?foo=bar")
    (check-equal? (url->path (string->url "http://example.com/test?foo=bar"))
                  "/test?foo=bar")
    (check-equal? (url->path (string->url "http://example.com/test?foo=bar&baz=qux"))
                  "/test?foo=bar&baz=qux"))
  (test-case "url-http?"
    (check-eq? (url-http? (string->url "http://localhost"))  #t)
    (check-eq? (url-http? (string->url "https://localhost")) #f))
  (test-case "url-https?"
    (check-equal? (url-https? (string->url "https://localhost")) #t)
    (check-equal? (url-https? (string->url "http://localhost"))  #f)))
