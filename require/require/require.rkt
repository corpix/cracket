#lang racket
(provide reprovide)

(define-syntax-rule (reprovide expr ...)
  (begin
    (require expr ...)
    (provide (all-from-out expr ...))))
