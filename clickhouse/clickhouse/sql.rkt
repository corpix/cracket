#lang racket
(require (for-syntax racket/base
                     corpix/syntax
                     "syntax.rkt")
         "emit.rkt"
         "type.rkt")
(provide clickhouse-sql-statement->string
         clickhouse-create-table
         clickhouse-insert
         clickhouse-select
         clickhouse-type
         clickhouse-function
         clickhouse-columns
         clickhouse-sql

         (all-from-out "emit.rkt"))

(define (clickhouse-sql-statement->string s)
  (clickhouse-emit-statement s))

;;

(define-syntax (clickhouse-create-table stx)
  (syntax-parse stx
    (c:ClickhouseCreateTable #`(clickhouse-sql-statement (quasiquote #,(attribute c.ast))))))

(define-syntax (clickhouse-insert stx)
  (syntax-parse stx
    (c:ClickhouseInsert #`(clickhouse-sql-statement (quasiquote #,(attribute c.ast))))))

(define-syntax (clickhouse-select stx)
  (syntax-parse stx
    (c:ClickhouseSelect #`(clickhouse-sql-statement (quasiquote #,(attribute c.ast))))))

(define-syntax (clickhouse-type stx)
  (syntax-parse stx
    ((_ t:ClickhouseType) #`(quasiquote #,(attribute t.ast)))))

(define-syntax (clickhouse-function stx)
  (syntax-parse stx
    ((_ fn:ClickhouseFunction) #`(quasiquote #,(attribute fn.ast)))))

(define-syntax (clickhouse-columns stx)
  (syntax-parse stx
    ((_ cols:ClickhouseColumns) #`(quasiquote #,(attribute cols.ast)))))

(define-syntax (clickhouse-sql stx)
  (syntax-parse stx
    ((_ (name rest ...))
     (with-syntax ((prefixed-name (format-id #'name "clickhouse-~a" #'name)))
       (syntax/loc stx
         (clickhouse-sql-statement->string (prefixed-name rest ...)))))))

;;

(module+ test
  (require rackunit
           syntax/parse)

  (test-case "create table"
    (check-equal?
     (clickhouse-sql (create-table numbers #:columns (n UInt8) (t String) #:engine Memory))
     "CREATE TABLE numbers (n UInt8, t String) ENGINE = Memory")

    (check-equal?
     (clickhouse-sql (create-table numbers #:columns (t (FixedString 16)) #:engine Memory))
     "CREATE TABLE numbers (t FixedString(16)) ENGINE = Memory")

    (check-equal?
     (clickhouse-sql (create-table numbers #:columns (t (Array UInt8)) #:engine Memory))
     "CREATE TABLE numbers (t Array(UInt8)) ENGINE = Memory")
    (check-equal?
     (clickhouse-sql (create-table numbers #:columns (t (Array (Array (FixedString 16)))) #:engine Memory))
     "CREATE TABLE numbers (t Array(Array(FixedString(16)))) ENGINE = Memory")

    (check-equal?
     (clickhouse-sql (create-table numbers #:columns (t (Tuple UInt8 UInt32)) #:engine Memory))
     "CREATE TABLE numbers (t Tuple(UInt8, UInt32)) ENGINE = Memory")

    (check-equal?
     (clickhouse-sql (create-table numbers #:columns (t (Enum8 ("foo" 1))) #:engine Memory))
     "CREATE TABLE numbers (t Enum8('foo' = 1)) ENGINE = Memory")
    (check-equal?
     (clickhouse-sql (create-table numbers #:columns (t (Enum8 ("foo" 1) ("bar" 2))) #:engine Memory))
     "CREATE TABLE numbers (t Enum8('foo' = 1, 'bar' = 2)) ENGINE = Memory")
    (check-equal?
     (clickhouse-sql (create-table numbers #:columns (t (Enum16 ("foo" 1) ("bar" 2))) #:engine Memory))
     "CREATE TABLE numbers (t Enum16('foo' = 1, 'bar' = 2)) ENGINE = Memory")

    (check-equal?
     (clickhouse-sql (create-table numbers #:columns (t (Nested (x UInt8) (y (Array UInt32)))) #:engine Memory))
     "CREATE TABLE numbers (t Nested(x UInt8, y Array(UInt32))) ENGINE = Memory")

    (check-equal?
     (clickhouse-sql (create-table "numbers" #:columns (n UInt8) (t String) #:engine Memory))
     "CREATE TABLE 'numbers' (n UInt8, t String) ENGINE = Memory")
    (check-equal?
     (let ((table 'dynamic)
           (cluster 'lancaster))
       (clickhouse-sql (create-table ,table #:cluster ,cluster #:columns (n UInt8) (t String) #:engine Memory)))
     "CREATE TABLE dynamic ON CLUSTER lancaster (n UInt8, t String) ENGINE = Memory")

    (check-equal?
     (clickhouse-sql (create-table numbers #:columns (t Date) #:engine (MergeTree t (t) 8192)))
     "CREATE TABLE numbers (t Date) ENGINE = MergeTree(t, (t), 8192)")
    (check-equal?
     (clickhouse-sql (create-table numbers #:columns (t Date) (d UInt8) #:engine (MergeTree t (intHash32 d) (tuple t (intHash32 d)) 8192)))
     "CREATE TABLE numbers (t Date, d UInt8) ENGINE = MergeTree(t, intHash32(d), tuple(t, intHash32(d)), 8192)")

    (check-equal?
     (clickhouse-sql (create-table numbers
                                          #:columns (t Date)
                                          #:engine (MergeTree)
                                          #:partition-by (toYYYYMMDD t)
                                          #:order-by t
                                          #:primary-key t
                                          #:sample-by (intHash32 t)))
     "CREATE TABLE numbers (t Date) ENGINE = MergeTree() PARTITION BY toYYYYMMDD(t) ORDER BY t PRIMARY KEY t SAMPLE BY intHash32(t)")
    (check-equal?
     (clickhouse-sql (create-table "numbers" #:columns ,'((n . UInt8) (t . String)) #:engine Memory))
     "CREATE TABLE 'numbers' (n UInt8, t String) ENGINE = Memory"))

  (test-case "insert"
    (check-equal?
     (clickhouse-sql (insert #:into numbers #:columns (n t) #:rows (1 "first")))
     "INSERT INTO numbers (n, t) VALUES (1, 'first')")
    (check-equal?
     (let ((n 2)
           (t "ehlo"))
       (clickhouse-sql (insert #:into numbers #:columns (n t) #:rows (,n ,t))))
     "INSERT INTO numbers (n, t) VALUES (2, 'ehlo')")
    (check-equal?
     (clickhouse-sql (insert #:into numbers #:rows (1 "hello") (2 "world") (3 "!")))
     "INSERT INTO numbers VALUES (1, 'hello'), (2, 'world'), (3, '!')")
    (check-equal?
     (clickhouse-sql (insert #:into numbers #:rows (1 "hello" (1 "hey"))))
     "INSERT INTO numbers VALUES (1, 'hello', (1, 'hey'))")
    (let ((vec (list->vector '((1 "hello") (2 "world") (3 "!")))))
      (check-equal?
       (clickhouse-sql (insert #:into numbers #:rows ,vec))
       "INSERT INTO numbers VALUES (1, 'hello'), (2, 'world'), (3, '!')"))

    (check-exn
     exn:fail:syntax?
     (lambda ()
       (syntax-parse #'(insert #:into numbers)))))

  (test-case "select"
    (check-equal?
     (clickhouse-sql (select 1))
     "SELECT 1")
    (check-equal?
     (clickhouse-sql (select (+ 1 1)))
     "SELECT 1 + 1")
    (check-equal?
     (clickhouse-sql (select (+ 1 1) #:as r))
     "SELECT 1 + 1 AS r")
    (check-equal?
     (clickhouse-sql (select (tuple 1 2 3)))
     "SELECT tuple(1, 2, 3)")
    (check-equal?
     (clickhouse-sql (select (tuple 1 2 3) (SHA256 "foo")))
     "SELECT tuple(1, 2, 3), SHA256('foo')")
    (check-equal?
     (clickhouse-sql (select (tuple 1 2 3) #:as t (SHA256 "foo") #:as h))
     "SELECT tuple(1, 2, 3) AS t, SHA256('foo') AS h")
    (check-equal?
     (clickhouse-sql (select time #:as t #:from test))
     "SELECT time AS t FROM test")
    (check-equal?
     (clickhouse-sql (select foo #:as f #:from (select (SHA256 "foo") #:as foo)))
     "SELECT foo AS f FROM (SELECT SHA256('foo') AS foo)")
    (check-equal?
     (clickhouse-sql (select foo bar #:from x #:where (and (= foo 1) (= bar "hello"))))
     "SELECT foo, bar FROM x WHERE and(foo = 1, bar = 'hello')")

    (check-equal?
     (clickhouse-sql (select foo #:from bar #:where (= foo "'bar")))
     "SELECT foo FROM bar WHERE foo = '\\'bar'")
    (check-equal?
     (clickhouse-sql (select ,'f\\oo #:from bar))
     "SELECT f\\\\oo FROM bar"))

  ;;

  (test-case "type"
    (check-equal? (clickhouse-type UInt8) (clickhouse-sql-type 'UInt8 null))
    (check-equal? (clickhouse-type (Tuple UInt16 (FixedString 8)))
                  (clickhouse-sql-type
                   'Tuple
                   (list (clickhouse-sql-type 'UInt16 null)
                         (clickhouse-sql-type 'FixedString
                                   (list (clickhouse-sql-expression #f (clickhouse-sql-parameter 8))))))))

  (test-case "function"
    (check-equal? (clickhouse-function (count)) (clickhouse-sql-function 'count null))
    (check-equal? (clickhouse-function (tuple 1 2 3))
                  (clickhouse-sql-function
                   'tuple
                   (list (clickhouse-sql-expression #f (clickhouse-sql-parameter 1))
                         (clickhouse-sql-expression #f (clickhouse-sql-parameter 2))
                         (clickhouse-sql-expression #f (clickhouse-sql-parameter 3)))))
    (let ((v 666))
      (check-equal? (clickhouse-function (tuple ,v))
                    (clickhouse-sql-function 'tuple
                                  (list (clickhouse-sql-expression #f (clickhouse-sql-parameter 666)))))))

  (test-case "columns"
    (check-equal? (clickhouse-columns (id UInt8) (t (Tuple UInt32)))
                  (list (clickhouse-sql-column 'id (clickhouse-sql-type 'UInt8 null))
                        (clickhouse-sql-column 't  (clickhouse-sql-type 'Tuple (list (clickhouse-sql-type 'UInt32 null))))))))
