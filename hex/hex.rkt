#lang racket/base
(require racket/contract/base)
(provide hex-char?
         hex-string?
         (contract-out
          (hex-char-code       (-> (integer-in 0 15) exact-nonnegative-integer?))
          (integer->hex-char   (-> (integer-in 0 15) char?))
          (hex-char->integer   (-> hex-char? integer?))
          (bytes->hex-string   (-> bytes? string?))
          (hex-string->bytes   (-> (and/c hex-string? string-length-even?) bytes?))
          (integer->hex-string (-> exact-nonnegative-integer? string?))
          (hex-string->integer (-> hex-string? integer?))))

(define hex-char?
  (or/c (char-in #\0 #\9)
        (char-in #\a #\f)
        (char-in #\A #\F)))

(define hex-string?
  (and/c string?
         (lambda (s)
           (andmap hex-char? (string->list s)))))

(define (string-length-even? v)
  (even? (string-length v)))

(define (hex-char-code n)
  (if (< n 10)
      (+ n (char->integer #\0))
      (+ n (- (char->integer #\a) 10))))

(define (integer->hex-char n)
  (integer->char (hex-char-code n)))

(define (hex-char->integer c)
  (cond ((char<=? #\0 c #\9) (- (char->integer c) (char->integer #\0)))
        ((char<=? #\a c #\f) (+ 10 (- (char->integer c) (char->integer #\a))))
        ((char<=? #\A c #\F) (+ 10 (- (char->integer c) (char->integer #\A))))))

(define (bytes->hex-string b)
  (let* ((size (bytes-length b))
         (buf (make-bytes (+ size size))))
    (for ((i (in-range size)))
      (let ((c (bytes-ref b i)))
        (bytes-set! buf (+ i i)   (hex-char-code (arithmetic-shift c -4))) ;; high
        (bytes-set! buf (+ i i 1) (hex-char-code (bitwise-and c #xF)))))   ;; low
    (bytes->string/latin-1 buf)))

(define (hex-string->bytes s)
  (let* ((size (quotient (string-length s) 2))
         (buf (make-bytes size)))
    (for ((i (in-range size)))
      (let ((high (hex-char->integer (string-ref s (+ i i))))
            (low  (hex-char->integer (string-ref s (+ i i 1)))))
        (bytes-set! buf i (+ (arithmetic-shift high 4) low))))
    buf))

(define (integer->hex-string n)
  (list->string
   (let loop ((acc null) (n n))
     (if (= n 0)
         acc
         (loop (cons (integer->hex-char (bitwise-and n 15)) acc)
               (arithmetic-shift n -4))))))

(define (hex-string->integer s)
  (for/fold ((acc 0))
            ((i (in-range (string-length s))))
    (+ (arithmetic-shift acc 4)
       (hex-char->integer (string-ref s i)))))

(module+ test
  (require rackunit)

  (test-case "hex-string?"
    (check-true  (hex-string? "0123456789abcdef"))
    (check-true  (hex-string? "ABC"))
    (check-false (hex-string? "abcdeoops"))
    (check-false (hex-string? #"oops")))

  (test-case "string-length-even?"
    (check-true  (string-length-even? ""))
    (check-true  (string-length-even? "abcd"))
    (check-false (string-length-even? "abcde")))

  (test-case "hex-char-code"
    (check-equal? (hex-char-code 0) 48)
    (check-equal? (hex-char-code 1) 49)
    (check-equal? (hex-char-code 9) 57)
    (check-equal? (hex-char-code 10) 97)
    (check-equal? (hex-char-code 15) 102)
    ;; XXX: invalid input is covered with contract
    ;; only for export
    (check-equal? (hex-char-code 16) 103))

  (test-case "hex-char->integer"
    (check-equal? (hex-char->integer #\0) 0)
    (check-equal? (hex-char->integer #\a) 10)
    (check-equal? (hex-char->integer #\f) 15)
    (check-equal? (hex-char->integer #\A) 10)
    (check-equal? (hex-char->integer #\F) 15)
    ;; XXX: invalid input is covered with contract
    ;; only for export
    (check-equal? (hex-char->integer #\x) (void)))

  (test-case "bytes->hex-string"
    (check-equal? (bytes->hex-string #"") "")
    (check-equal? (bytes->hex-string #"hello") "68656c6c6f")
    (check-equal? (bytes->hex-string #"hello world") "68656c6c6f20776f726c64"))

  (test-case "hex-string->bytes"
    (check-equal? (hex-string->bytes "d3adbeef") #"\323\255\276\357"))

  (test-case "integer->hex-string"
    (check-equal? (integer->hex-string 3551376191) "d3adb33f")
    (check-equal? (integer->hex-string 221961011)  "d3adb33"))

  (test-case "hex-string->integer"
    (check-equal? (hex-string->integer "d3adb33f") 3551376191)
    (check-equal? (hex-string->integer "d3adb33")  221961011)))
