#lang racket
(provide (all-defined-out)
         #%app) ;; FIXME: wtf? #%app

(define escape-regexp (regexp "([\\'])"))
(define (sql-escape v)
  (regexp-replace* escape-regexp v "\\\\\\1"))

(module+ test
  (require rackunit)
  (test-case "sql-escape"
    (check-equal? (sql-escape "'") "\\'")
    (check-equal? (sql-escape "\\ '\"") "\\\\ \\'\"")
    (check-equal? (sql-escape "hello()") "hello()")))
