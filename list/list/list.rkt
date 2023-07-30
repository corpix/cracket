#lang racket
(provide values->list)

(define-syntax (values->list stx)
  (syntax-case stx ()
    ((_ expr) (syntax (call-with-values (lambda () expr) list)))))
