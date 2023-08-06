#lang racket
(provide values->list
         enumerate)

(define-syntax (values->list stx)
  (syntax-case stx ()
    ((_ expr) (syntax (call-with-values (lambda () expr) list)))))

(define (enumerate lst)
  (for/list ((value (in-list lst))
             (index (in-naturals)))
    (list value index)))
