#lang racket
(require (for-syntax racket/base
                     syntax/parse
                     "syntax.rkt")
         "emit.rkt"
         "type.rkt")
(provide sql:statement->string
         create-table
         insert
         select
         type
         function
         columns
         (all-from-out "emit.rkt"))

(define (sql:statement->string s)
  (emit-statement s))

;;

(define-syntax (create-table stx)
  (syntax-parse stx
    (c:CreateTable #`(sql:statement (quasiquote #,(attribute c.ast))))))

(define-syntax (insert stx)
  (syntax-parse stx
    (c:Insert #`(sql:statement (quasiquote #,(attribute c.ast))))))

(define-syntax (select stx)
  (syntax-parse stx
    (c:Select #`(sql:statement (quasiquote #,(attribute c.ast))))))

(define-syntax (type stx)
  (syntax-parse stx
    ((_ t:Type) #`(quasiquote #,(attribute t.ast)))))

(define-syntax (function stx)
  (syntax-parse stx
    ((_ fn:Function) #`(quasiquote #,(attribute fn.ast)))))

(define-syntax (columns stx)
  (syntax-parse stx
    ((_ cols:Columns) #`(quasiquote #,(attribute cols.ast)))))

;;

(module+ test
  (require rackunit
           syntax/parse)

  (test-case "create table"
    (check-equal?
     (sql:statement->string (create-table numbers #:columns (n UInt8) (t String) #:engine Memory))
     "CREATE TABLE numbers (n UInt8, t String) ENGINE = Memory")

    (check-equal?
     (sql:statement->string (create-table numbers #:columns (t (FixedString 16)) #:engine Memory))
     "CREATE TABLE numbers (t FixedString(16)) ENGINE = Memory")

    (check-equal?
     (sql:statement->string (create-table numbers #:columns (t (Array UInt8)) #:engine Memory))
     "CREATE TABLE numbers (t Array(UInt8)) ENGINE = Memory")
    (check-equal?
     (sql:statement->string (create-table numbers #:columns (t (Array (Array (FixedString 16)))) #:engine Memory))
     "CREATE TABLE numbers (t Array(Array(FixedString(16)))) ENGINE = Memory")

    (check-equal?
     (sql:statement->string (create-table numbers #:columns (t (Tuple UInt8 UInt32)) #:engine Memory))
     "CREATE TABLE numbers (t Tuple(UInt8, UInt32)) ENGINE = Memory")

    (check-equal?
     (sql:statement->string (create-table numbers #:columns (t (Enum8 ("foo" 1))) #:engine Memory))
     "CREATE TABLE numbers (t Enum8('foo' = 1)) ENGINE = Memory")
    (check-equal?
     (sql:statement->string (create-table numbers #:columns (t (Enum8 ("foo" 1) ("bar" 2))) #:engine Memory))
     "CREATE TABLE numbers (t Enum8('foo' = 1, 'bar' = 2)) ENGINE = Memory")
    (check-equal?
     (sql:statement->string (create-table numbers #:columns (t (Enum16 ("foo" 1) ("bar" 2))) #:engine Memory))
     "CREATE TABLE numbers (t Enum16('foo' = 1, 'bar' = 2)) ENGINE = Memory")

    (check-equal?
     (sql:statement->string (create-table numbers #:columns (t (Nested (x UInt8) (y (Array UInt32)))) #:engine Memory))
     "CREATE TABLE numbers (t Nested(x UInt8, y Array(UInt32))) ENGINE = Memory")

    (check-equal?
     (sql:statement->string (create-table "numbers" #:columns (n UInt8) (t String) #:engine Memory))
     "CREATE TABLE 'numbers' (n UInt8, t String) ENGINE = Memory")
    (check-equal?
     (let ((table 'dynamic)
           (cluster 'lancaster))
       (sql:statement->string (create-table ,table #:cluster ,cluster #:columns (n UInt8) (t String) #:engine Memory)))
     "CREATE TABLE dynamic ON CLUSTER lancaster (n UInt8, t String) ENGINE = Memory")

    (check-equal?
     (sql:statement->string (create-table numbers #:columns (t Date) #:engine (MergeTree t (t) 8192)))
     "CREATE TABLE numbers (t Date) ENGINE = MergeTree(t, (t), 8192)")
    (check-equal?
     (sql:statement->string (create-table numbers #:columns (t Date) (d UInt8) #:engine (MergeTree t (intHash32 d) (tuple t (intHash32 d)) 8192)))
     "CREATE TABLE numbers (t Date, d UInt8) ENGINE = MergeTree(t, intHash32(d), tuple(t, intHash32(d)), 8192)")

    (check-equal?
     (sql:statement->string (create-table numbers
                                          #:columns (t Date)
                                          #:engine (MergeTree)
                                          #:partition-by (toYYYYMMDD t)
                                          #:order-by t
                                          #:primary-key t
                                          #:sample-by (intHash32 t)))
     "CREATE TABLE numbers (t Date) ENGINE = MergeTree() PARTITION BY toYYYYMMDD(t) ORDER BY t PRIMARY KEY t SAMPLE BY intHash32(t)")
    (check-equal?
     (sql:statement->string (create-table "numbers" #:columns ,'((n . UInt8) (t . String)) #:engine Memory))
     "CREATE TABLE 'numbers' (n UInt8, t String) ENGINE = Memory"))

  (test-case "insert"
    (check-equal?
     (sql:statement->string (insert #:into numbers #:columns (n t) #:rows (1 "first")))
     "INSERT INTO numbers (n, t) VALUES (1, 'first')")
    (check-equal?
     (let ((n 2)
           (t "ehlo"))
       (sql:statement->string (insert #:into numbers #:columns (n t) #:rows (,n ,t))))
     "INSERT INTO numbers (n, t) VALUES (2, 'ehlo')")
    (check-equal?
     (sql:statement->string (insert #:into numbers #:rows (1 "hello") (2 "world") (3 "!")))
     "INSERT INTO numbers VALUES (1, 'hello'), (2, 'world'), (3, '!')")
    (let ((vec (list->vector '((1 "hello") (2 "world") (3 "!")))))
      (check-equal?
       (sql:statement->string (insert #:into numbers #:rows ,vec))
       "INSERT INTO numbers VALUES (1, 'hello'), (2, 'world'), (3, '!')"))

    (check-exn
     exn:fail:syntax?
     (lambda ()
       (syntax-parse #'(insert #:into numbers)))))

  (test-case "select"
    (check-equal?
     (sql:statement->string (select 1))
     "SELECT 1")
    (check-equal?
     (sql:statement->string (select (+ 1 1)))
     "SELECT 1 + 1")
    (check-equal?
     (sql:statement->string (select (+ 1 1) #:as r))
     "SELECT 1 + 1 AS r")
    (check-equal?
     (sql:statement->string (select (tuple 1 2 3)))
     "SELECT tuple(1, 2, 3)")
    (check-equal?
     (sql:statement->string (select (tuple 1 2 3) (SHA256 "foo")))
     "SELECT tuple(1, 2, 3), SHA256('foo')")
    (check-equal?
     (sql:statement->string (select (tuple 1 2 3) #:as t (SHA256 "foo") #:as h))
     "SELECT tuple(1, 2, 3) AS t, SHA256('foo') AS h")
    (check-equal?
     (sql:statement->string (select time #:as t #:from test))
     "SELECT time AS t FROM test")
    (check-equal?
     (sql:statement->string (select foo #:as f #:from (select (SHA256 "foo") #:as foo)))
     "SELECT foo AS f FROM (SELECT SHA256('foo') AS foo)")
    (check-equal?
     (sql:statement->string (select foo bar #:from x #:where (and (= foo 1) (= bar "hello"))))
     "SELECT foo, bar FROM x WHERE and(foo = 1, bar = 'hello')")

    (check-equal?
     (sql:statement->string (select foo #:from bar #:where (= foo "'bar")))
     "SELECT foo FROM bar WHERE foo = '\\'bar'")
    (check-equal?
     (sql:statement->string (select ,'f\\oo #:from bar))
     "SELECT f\\\\oo FROM bar"))

  ;;

  (test-case "type"
    (check-equal? (type UInt8) (sql:type 'UInt8 null))
    (check-equal? (type (Tuple UInt16 (FixedString 8)))
                  (sql:type
                   'Tuple
                   (list (sql:type 'UInt16 null)
                         (sql:type 'FixedString
                                   (list (sql:expression #f (sql:parameter 8))))))))

  (test-case "function"
    (check-equal? (function (count)) (sql:function 'count null))
    (check-equal? (function (tuple 1 2 3))
                  (sql:function
                   'tuple
                   (list (sql:expression #f (sql:parameter 1))
                         (sql:expression #f (sql:parameter 2))
                         (sql:expression #f (sql:parameter 3)))))
    (let ((v 666))
      (check-equal? (function (tuple ,v))
                    (sql:function 'tuple
                                  (list (sql:expression #f (sql:parameter 666)))))))

  (test-case "columns"
    (check-equal? (columns (id UInt8) (t (Tuple UInt32)))
                  (list (sql:column 'id (sql:type 'UInt8 null))
                        (sql:column 't  (sql:type 'Tuple (list (sql:type 'UInt32 null))))))))
