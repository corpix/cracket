#lang racket
(require racket/string)
(provide (all-from-out racket/string)
         *->string
         string->*)

(define (*->string value)
  (with-output-to-string (lambda () (write value))))

(define (string->* value)
  (with-input-from-string value read))
