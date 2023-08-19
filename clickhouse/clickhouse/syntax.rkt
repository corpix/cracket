#lang racket
(require corpix/syntax
         (for-syntax corpix/syntax
                     "type.rkt"
                     "constant.rkt")
         "type.rkt"
         "emit.rkt")
(provide (all-defined-out))

(begin-for-syntax
  (define-syntax-class ClickhouseUnquote
    (pattern ((~datum unquote) ast:expr)))

  (define-syntax-class ClickhouseUnquoteSplicing
    (pattern ((~datum unquote-splicing) ast:expr)))

  (define-syntax-class ClickhouseParameter
    (pattern v:id #:with ast #'(make-clickhouse-sql-parameter 'v))
    (pattern v:string #:with ast #'(make-clickhouse-sql-parameter v))
    (pattern v:number #:with ast #'(make-clickhouse-sql-parameter v))
    (pattern v:boolean #:with ast #'(make-clickhouse-sql-parameter v))
    (pattern v:ClickhouseUnquote #:with ast #`(make-clickhouse-sql-parameter #,(attribute v.ast))))

  (define-splicing-syntax-class ClickhouseTypeArguments
    #:description "type argument"
    (pattern (~seq argument:ClickhouseType ...) #:with ast (attribute argument.ast))
    (pattern (~seq argument:ClickhouseColumn ...) #:with ast (attribute argument.ast))
    (pattern (~seq argument:ClickhouseExpression ...) #:with ast (attribute argument.ast)))

  (define-syntax-class ClickhouseType
    #:description "type"
    (pattern (id:id (key:ClickhouseExpressionAnonymous value:ClickhouseExpression) ...)
             #:when (clickhouse-mapping-type? (syntax->datum #'id))
             #:fail-when (and (not (clickhouse-complex-type? (syntax->datum #'id))) #'id)
             "illegal complex type name"
             #:with ast #`(make-clickhouse-sql-type 'id
                                                    (list (make-clickhouse-sql-mapping key.ast value.ast) ...)))
    (pattern (id:id arguments:ClickhouseTypeArguments)
             #:fail-when
             (and (not (clickhouse-complex-type? (syntax->datum #'id))) #'id)
             "illegal complex type name"
             #:with ast #`(make-clickhouse-sql-type 'id (list #,@(attribute arguments.ast))))
    (pattern id:id
             #:fail-when
             (and (not (clickhouse-primitive-type? (syntax->datum #'id))) #'id)
             "illegal primitive type name"
             #:with ast #'(make-clickhouse-sql-type 'id null)))

  (define-syntax-class ClickhouseColumn
    #:description "table column"
    (pattern (name:id type:ClickhouseType)
             #:with ast #`(make-clickhouse-sql-column 'name #,(attribute type.ast))))

  (define-splicing-syntax-class ClickhouseColumns
    #:description "table columns list"
    (pattern (~seq columns:ClickhouseColumn ...) #:with ast (attribute columns.ast))
    (pattern v:ClickhouseUnquote #:with ast #'v))

  (define-syntax-class ClickhouseExpressionAnonymous
    (pattern expression:ClickhouseParameter
             #:with ast #`(make-clickhouse-sql-expression #f #,(attribute expression.ast)))
    (pattern expression:ClickhouseFunction
             #:with ast #`(make-clickhouse-sql-expression #f #,(attribute expression.ast)))
    (pattern expression:ClickhouseTuple
             #:with ast #`(make-clickhouse-sql-expression #f #,(attribute expression.ast))))
  (define-splicing-syntax-class ClickhouseExpression
    (pattern expression:ClickhouseExpressionAnonymous
             #:with ast (attribute expression.ast))
    (pattern (~seq expression:ClickhouseExpressionAnonymous #:as namexpression:ClickhouseParameter)
             #:with ast #`(make-clickhouse-sql-expression #,(attribute namexpression.ast)
                                                          #,(attribute expression.ast))))
  (define-syntax-class ClickhouseExpressions
    (pattern expr:ClickhouseUnquote
             #:with ast #`(#,(attribute expr.ast)))
    (pattern (exprs:ClickhouseExpression ...)
             #:with ast (attribute exprs.ast)))

  (define-syntax-class ClickhouseFunction
    (pattern (id:id arguments:ClickhouseExpression ...)
             #:fail-when (and (not (clickhouse-function? (syntax->datum #'id))) #'id) "expected function"
             #:with ast #`(make-clickhouse-sql-function 'id (list #,@(attribute arguments.ast)))))

  (define-syntax-class ClickhouseTuple
    (pattern (e:ClickhouseExpression ...) #:with ast #`(make-clickhouse-sql-tuple (list #,@(attribute e.ast)))))

  (define-syntax-class ClickhouseEngine
    #:description "table engine"
    (pattern engine:id
             #:fail-when (and (not (clickhouse-primitive-engine? (syntax->datum #'engine))) #'engine)
             "illegal engine name"
             #:with ast #'(make-clickhouse-sql-engine 'engine #f))
    (pattern (engine:id arguments:ClickhouseExpressionAnonymous ...)
             #:fail-when (and (not (clickhouse-complex-engine? (syntax->datum #'engine)))
                              #'engine)
             "illegal engine name"
             #:with ast #`(make-clickhouse-sql-engine 'engine (list #,@(attribute arguments.ast)))))

  (define-splicing-syntax-class ClickhouseWhere
    #:description "where clause"
    (pattern (~seq #:where e:ClickhouseExpression) #:with ast (attribute e.ast))
    (pattern (~seq) #:with ast #'#f))

  (define-syntax-class ClickhouseCreateTable
    #:description "create table"
    (pattern (_ (~optional (~and #:temporary temporary?))
                (~optional (~seq (~and #:if-not-exists if-not-exists?)))
                name:ClickhouseParameter
                (~optional (~seq #:cluster cluster:ClickhouseParameter))
                #:columns columns:ClickhouseColumns
                #:engine engine:ClickhouseEngine
                (~optional (~seq #:partition-by partition-by:ClickhouseExpression))
                (~optional (~seq #:order-by order-by:ClickhouseExpression))
                (~optional (~seq #:primary-key primary-key:ClickhouseExpression))
                (~optional (~seq #:sample-by sample-by:ClickhouseExpression)))
             #:with ast #`(make-clickhouse-sql-create-table
                           #,(attribute name.ast)
                           #,(if (attribute temporary?) #'#t #'#f)
                           #,(if (attribute if-not-exists?) #'#t #'#f)
                           #,(attribute cluster.ast)
                           (list #,@(attribute columns.ast))
                           #,(attribute engine.ast)
                           #,(attribute partition-by.ast)
                           #,(attribute order-by.ast)
                           #,(attribute primary-key.ast)
                           #,(attribute sample-by.ast))))

  (define-syntax-class ClickhouseInsert
    #:description "insert statement"
    (pattern (_ #:into name:ClickhouseParameter
                (~optional (~seq #:columns columns:ClickhouseExpression ...))
                (~seq #:rows rows:ClickhouseExpressions))
             #:with ast #`(make-clickhouse-sql-insert
                           #,(attribute name.ast)
                           #,(if (attribute columns.ast) #`(list #,@(attribute columns.ast)) #'#f)
                           (list #,@(attribute rows.ast)))))

  (define-syntax-class ClickhouseSelect
    #:description "select statement"
    (pattern (_ expr:ClickhouseExpression ...
                (~optional (~or (~seq #:from from:ClickhouseParameter)
                                (~seq #:from from:ClickhouseSelect)))
                (~optional (~seq #:where where:ClickhouseExpression)))
             #:with ast #`(make-clickhouse-sql-select
                           (list #,@(attribute expr.ast))
                           #,(attribute from.ast)
                           #,(attribute where.ast)))))

;;

(define-syntax (clickhouse-create-table stx)
  (syntax-parse stx
    (c:ClickhouseCreateTable
     (quasisyntax/loc stx
       (make-clickhouse-sql-statement #,(attribute c.ast))))))

(define-syntax (clickhouse-insert stx)
  (syntax-parse stx
    (c:ClickhouseInsert
     (quasisyntax/loc stx
       (make-clickhouse-sql-statement #,(attribute c.ast))))))

(define-syntax (clickhouse-select stx)
  (syntax-parse stx
    (c:ClickhouseSelect
     (quasisyntax/loc stx
       (make-clickhouse-sql-statement #,(attribute c.ast))))))

(define-syntax (clickhouse-type stx)
  (syntax-parse stx
    ((_ t:ClickhouseType) (attribute t.ast))))

(define-syntax (clickhouse-function stx)
  (syntax-parse stx
    ((_ fn:ClickhouseFunction) (attribute fn.ast))))

(define-syntax (clickhouse-columns stx)
  (syntax-parse stx
    ((_ cols:ClickhouseColumns) #`(list #,@(attribute cols.ast)))))

(define-syntax (clickhouse-ast stx)
  (syntax-parse stx
    ((_ (name rest ...))
     (with-syntax ((prefixed-name (format-id #'name "clickhouse-~a" #'name)))
       (syntax/loc stx
         (prefixed-name rest ...))))))

(define-syntax (clickhouse-sql stx)
  (syntax-parse stx
    ((_ expr)
     (syntax/loc stx
       (clickhouse-emit-statement (clickhouse-ast expr))))))

;;

(module+ test
  (require rackunit
           syntax/parse)

  (test-case "create table"
    (check-equal?
     (clickhouse-sql (create-table numbers #:columns (n1 UInt8) (t1 String) #:engine Memory))
     "CREATE TABLE numbers (n1 UInt8,t1 String) ENGINE = Memory")

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
     "CREATE TABLE numbers (t Tuple(UInt8,UInt32)) ENGINE = Memory")

    (check-equal?
     (clickhouse-sql (create-table numbers #:columns (t (Enum8 ("foo" 1))) #:engine Memory))
     "CREATE TABLE numbers (t Enum8('foo' = 1)) ENGINE = Memory")
    (check-equal?
     (clickhouse-sql (create-table numbers #:columns (t (Enum8 ("foo" 1) ("bar" 2))) #:engine Memory))
     "CREATE TABLE numbers (t Enum8('foo' = 1,'bar' = 2)) ENGINE = Memory")
    (check-equal?
     (clickhouse-sql (create-table numbers #:columns (t (Enum16 ("foo" 1) ("bar" 2))) #:engine Memory))
     "CREATE TABLE numbers (t Enum16('foo' = 1,'bar' = 2)) ENGINE = Memory")
    ;; FIXME: (unquote splicing)
    ;; (check-equal?
    ;;  (let ((enum '(("foo" 1) ("bar" 2))))
    ;;    (clickhouse-create-table numbers #:columns (t (Enum8 ,@enum)) #:engine Memory)
    ;;    ;; (clickhouse-sql (create-table numbers #:columns (t (Enum8 ,@enum)) #:engine Memory))
    ;;    )
    ;;  "CREATE TABLE numbers (t Enum8('foo' = 1)) ENGINE = Memory")

    (check-equal?
     (clickhouse-sql (create-table numbers #:columns (t (Nested (x UInt8) (y (Array UInt32)))) #:engine Memory))
     "CREATE TABLE numbers (t Nested(x UInt8,y Array(UInt32))) ENGINE = Memory")

    (check-equal?
     (clickhouse-sql (create-table "numbers" #:columns (n UInt8) (t String) #:engine Memory))
     "CREATE TABLE 'numbers' (n UInt8,t String) ENGINE = Memory")
    (check-equal?
     (let ((table 'dynamic)
           (cluster 'test))
       (clickhouse-sql (create-table ,table #:cluster ,cluster #:columns (n UInt8) (t String) #:engine Memory)))
     "CREATE TABLE dynamic ON CLUSTER test (n UInt8,t String) ENGINE = Memory")

    (check-equal?
     (clickhouse-sql (create-table numbers #:columns (t Date) #:engine (MergeTree t (t) 8192)))
     "CREATE TABLE numbers (t Date) ENGINE = MergeTree(t,(t),8192)")
    (check-equal?
     (clickhouse-sql (create-table numbers
                                   #:columns (t Date) (d UInt8)
                                   #:engine (MergeTree
                                             t
                                             (intHash32 d)
                                             (tuple t (intHash32 d))
                                             8192)))
     "CREATE TABLE numbers (t Date,d UInt8) ENGINE = MergeTree(t,intHash32(d),tuple(t,intHash32(d)),8192)")

    (check-equal?
     (clickhouse-sql (create-table numbers
                                          #:columns (t Date)
                                          #:engine (MergeTree)
                                          #:partition-by (toYYYYMMDD t)
                                          #:order-by t
                                          #:primary-key t
                                          #:sample-by (intHash32 t)))
     "CREATE TABLE numbers (t Date) ENGINE = MergeTree() PARTITION BY toYYYYMMDD(t) ORDER BY t PRIMARY KEY t SAMPLE BY intHash32(t)")
    ;; FIXME
    ;; (check-equal?
    ;;  (clickhouse-sql (create-table "numbers" #:columns ,'((n . UInt8) (t . String)) #:engine Memory))
    ;;  "CREATE TABLE 'numbers' (n UInt8,t String) ENGINE = Memory")
    )

  (test-case "insert"
    (check-equal?
     (clickhouse-sql (insert #:into numbers #:columns (n t) #:rows ((1 "first"))))
     "INSERT INTO numbers (n,t) VALUES (1,'first')")
    (check-equal?
     (let ((n 2)
           (t "ehlo"))
       (clickhouse-sql (insert #:into numbers #:columns (n t) #:rows ((,n ,t)))))
     "INSERT INTO numbers (n,t) VALUES (2,'ehlo')")
    (check-equal?
     (clickhouse-sql (insert #:into numbers #:rows ((1 "hello") (2 "world") (3 "!"))))
     "INSERT INTO numbers VALUES (1,'hello'),(2,'world'),(3,'!')")
    (check-equal?
     (clickhouse-sql (insert #:into numbers #:rows ((1 "hello" (1 "hey")))))
     "INSERT INTO numbers VALUES (1,'hello',(1,'hey'))")

    (let ((rows '((1 "hello") (2 "world") (3 "!")))) ;; work here
      (check-equal?
       (clickhouse-sql (insert #:into numbers #:rows ,rows))
       "INSERT INTO numbers VALUES (1,'hello'),(2,'world'),(3,'!')"))

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
     "SELECT tuple(1,2,3)")
    (check-equal?
     (clickhouse-sql (select (tuple 1 2 3) (SHA256 "foo")))
     "SELECT tuple(1,2,3),SHA256('foo')")
    (check-equal?
     (clickhouse-sql (select (tuple 1 2 3) #:as t (SHA256 "foo") #:as h))
     "SELECT tuple(1,2,3) AS t,SHA256('foo') AS h")
    (check-equal?
     (clickhouse-sql (select time #:as t #:from test))
     "SELECT time AS t FROM test")
    (check-equal?
     (clickhouse-sql (select foo #:as f #:from (select (SHA256 "foo") #:as foo)))
     "SELECT foo AS f FROM (SELECT SHA256('foo') AS foo)")
    (check-equal?
     (clickhouse-sql (select foo bar #:from x #:where (and (= foo 1) (= bar "hello"))))
     "SELECT foo,bar FROM x WHERE and(foo = 1,bar = 'hello')")

    (check-equal?
     (clickhouse-sql (select foo #:from bar #:where (= foo "'bar")))
     "SELECT foo FROM bar WHERE foo = '\\'bar'")
    (check-equal?
     (clickhouse-sql (select ,'f\\oo #:from bar))
     "SELECT f\\\\oo FROM bar"))

  ;;

  (test-case "type"
    (check-equal? (clickhouse-type UInt8) (make-clickhouse-sql-type 'UInt8 null))
    (check-equal? (clickhouse-type (Tuple UInt16 (FixedString 8)))
                  (make-clickhouse-sql-type
                   'Tuple
                   (list (make-clickhouse-sql-type 'UInt16 null)
                         (make-clickhouse-sql-type 'FixedString
                                   (list (make-clickhouse-sql-expression #f (make-clickhouse-sql-parameter 8))))))))

  (test-case "function"
    (check-equal? (clickhouse-function (count)) (make-clickhouse-sql-function 'count null))
    (check-equal? (clickhouse-function (tuple 1 2 3))
                  (make-clickhouse-sql-function
                   'tuple
                   (list (make-clickhouse-sql-expression #f (make-clickhouse-sql-parameter 1))
                         (make-clickhouse-sql-expression #f (make-clickhouse-sql-parameter 2))
                         (make-clickhouse-sql-expression #f (make-clickhouse-sql-parameter 3)))))
    (let ((v 666))
      (check-equal? (clickhouse-function (tuple ,v))
                    (make-clickhouse-sql-function 'tuple
                                  (list (make-clickhouse-sql-expression #f (make-clickhouse-sql-parameter 666)))))))

  (test-case "columns"
    (check-equal? (clickhouse-columns (id UInt8) (t (Tuple UInt32)))
                  (list (make-clickhouse-sql-column 'id (make-clickhouse-sql-type 'UInt8 null))
                        (make-clickhouse-sql-column 't  (make-clickhouse-sql-type 'Tuple (list (make-clickhouse-sql-type 'UInt32 null))))))))
