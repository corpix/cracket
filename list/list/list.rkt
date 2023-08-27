#lang racket
(provide values->list
         enumerate
         assocv)

(define-syntax (values->list stx)
  (syntax-case stx ()
    ((_ expr) (syntax (call-with-values (lambda () expr) list)))))

(define (enumerate lst)
  (for/list ((value (in-list lst))
             (index (in-naturals)))
    (list value index)))

(define (assocv key lst (default #f))
  (let ((p (assoc key lst)))
    (if p (cdr p) default)))
