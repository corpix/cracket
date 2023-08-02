#lang racket
(require db
         deta
         corpix/strings)
(provide (all-from-out db)
         (all-from-out deta)
         (all-defined-out))

(define (db-insert-id result)
  (let* ((info (simple-result-info result))
         (pair (assoc 'insert-id info)))
    (and pair (cdr pair))))

(define-type sexpr
  #:declaration
  (lambda (type dialect)
    (case dialect
      ((sqlite3) "TEXT")
      ((postgresql) "TEXT")))
  #:load (lambda (_ __ value) (string->* value))
  #:dump (lambda (_ __ value) (*->string value)))

(define-type symbol
  #:declaration
  (lambda (type dialect)
    (case dialect
      ((sqlite3) "TEXT")
      ((postgresql) "TEXT")))
  #:load (lambda (_ __ value) (string->symbol value))
  #:dump (lambda (_ __ value) (symbol->string value)))
