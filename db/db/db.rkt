#lang racket
(require db
         deta)
(provide (all-from-out db)
         (all-from-out deta)
         db-insert-id)

(define (db-insert-id result)
  (let* ((info (simple-result-info result))
         (pair (assoc 'insert-id info)))
    (and pair (cdr pair))))
