#lang racket
(require racket/string)
(provide (all-from-out racket/string)
         *->string
         string->*
         string-join-sequence-with)

(define (*->string value)
  (with-output-to-string (lambda () (write value))))

(define (string->* value)
  (with-input-from-string value read))

(define (string-join-sequence-with seq proc sep)
  (for/fold ((acc ""))
            ((elem seq)
             (index (in-naturals)))
    (string-append
     (if (> index 0)
         (string-append acc sep)
         acc)
     (proc elem))))
