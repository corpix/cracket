#lang racket
(provide read-clickhouse)

;; FIXME: typed error
(define (expect ch . expected)
  (unless (memq ch expected)
    (error 'read "expected: ~v, got: ~a" expected ch))
  ch)

(define (expect-string port expected)
  (list->string (for/list ((ch expected))
                  (expect (read-char port) ch))))

(define (skip-whitespace port)
  (let ((ch (peek-char port)))
    (when (char-whitespace? ch)
      (read-char port)
      (skip-whitespace port))))

(define (in-port-until port reader done?)
  (make-do-sequence
   (lambda ()
     (values reader
             (lambda (port) port)
             port
             (lambda (port) (not (done? port)))
             (lambda values #t)
             (lambda (port . values) #t)))))

;;

(define (read-clickhouse (port (current-input-port)))
  (skip-whitespace port)
  (case (peek-char port)
    ((#\() (read/tuple  port))
    ((#\() (read/list   port))
    ((#\') (read/string port))
    ((#\\) (read/null   port))
    (else  (read/number port))))

(define (read/sequence start stop port)
  (expect (read-char port) start)
  (skip-whitespace port)
  (begin0
      (for/list ((value
                  (in-port-until
                   port
                   (lambda (port)
                     (skip-whitespace port)
                     (begin0 (read-clickhouse port)
                       (skip-whitespace port)
                       (expect (peek-char port) #\, stop)))
                   (lambda (port)
                     (eq? (peek-char port) stop)))))
        (when (eq? (peek-char port) #\,)
          (read-char port))
        value)
    (expect (read-char port) stop)))

(define (read/digits port)
  (let ((digits
         (for/list
             ((digit (in-port-until
                      port
                      read-char
                      (lambda (port)
                        (let ((ch (peek-char port)))
                          (or (eof-object? ch)
                              (not (or
                                    (char-numeric? ch)
                                    (char=? #\. ch)))))))))
           digit)))
    (when (and (null? digits) (eof-object? (peek-char port)))
      (error 'read "unexpected EOF"))
    (when (null? digits)
      (error 'read "expected: digits, got: ~a" (peek-char port)))
    digits))

(define (read/string port)
  (expect (read-char port) #\')
  (begin0
      (list->string
       (for/list
           ((ch (in-port-until
                 port
                 (lambda (port)
                   (let ((ch (read-char port)))
                     (when (eof-object? ch)
                       (error 'read "unexpected EOF"))
                     (if (eq? ch #\\)
                         (let ((esc (read-char port)))
                           (when (eof-object? ch)
                             (error 'read "unexpected EOF"))
                           (case esc
                             ((#\') #\')
                             (else esc)))
                         ch)))
                 (lambda (port)
                   (eq? (peek-char port) #\')))))
         ch))
    (expect (read-char port) #\')))

(define (read/number port)
  (let* ((sign (if (eq? (peek-char port) #\-)
                   (list (read-char port))
                   '()))
         (digits (read/digits port))
         (frac (if (eq? (peek-char port) #\.)
                   (list* (read-char port)
                          (read/digits port))
                   '())))
    (string->number
     (list->string
      (append sign digits frac)))))

(define (read/list  port) (read/sequence #\( #\) port))
(define (read/tuple port) (read/sequence #\( #\) port))
(define (read/null  port) (expect-string port "\\N") 'null)

(module+ test
  (require rackunit)

  (check-equal? (read-clickhouse (open-input-string "'hell\\'o'")) "hell'o")

  (check-equal? (read-clickhouse (open-input-string "123")) 123)
  (check-equal? (read-clickhouse (open-input-string "-123")) -123)
  (check-equal? (read-clickhouse (open-input-string "123.321")) 123.321)
  (check-equal? (read-clickhouse (open-input-string "-123.321")) -123.321)

  (check-equal? (read-clickhouse (open-input-string "'hello'")) "hello")

  (check-equal? (read-clickhouse (open-input-string "\\N")) 'null)

  (check-equal? (read-clickhouse (open-input-string "['foo']"))
                '("foo"))
  (check-equal? (read-clickhouse (open-input-string "['foo','bar']"))
                '("foo" "bar"))
  (check-equal? (read-clickhouse (open-input-string "['foo', 'bar']"))
                '("foo" "bar"))
  (check-equal? (read-clickhouse (open-input-string "[1, 2.1, -1, -2.1, 'hello']"))
                '(1 2.1 -1 -2.1 "hello"))

  (check-equal? (read-clickhouse (open-input-string "('foo')"))
                '("foo"))
  (check-equal? (read-clickhouse (open-input-string "('foo','bar')"))
                '("foo" "bar"))
  (check-equal? (read-clickhouse (open-input-string "('foo', 'bar')"))
                '("foo" "bar")))
