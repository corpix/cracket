#lang racket
(require "constant.rkt"
         "escape.rkt"
         "type.rkt")
(provide clickhouse-emit-statement
         clickhouse-emit-create-table
         clickhouse-emit-insert
         clickhouse-emit-select
         ;;
         clickhouse-emit-type
         clickhouse-emit-function
         clickhouse-emit-columns)

(define (clickhouse-emit-statement ast)
  (match (clickhouse-sql-statement-value ast)
    ((? clickhouse-sql-create-table? ast) (clickhouse-emit-create-table ast))
    ((? clickhouse-sql-insert?       ast) (clickhouse-emit-insert ast))
    ((? clickhouse-sql-select?       ast) (clickhouse-emit-select ast))))

(define (clickhouse-emit-value ast)
  (match ast
    ((? number?) (number->string ast))
    ((? symbol?) (sql-escape (symbol->string ast))) ;; FIXME: I don't think we need to escape symbols, this is ill logic
    ((? string?) (format "'~a'" (sql-escape ast)))))

(define (clickhouse-emit-parameter ast)
  (match ast
    ((clickhouse-sql-parameter expr) (clickhouse-emit-value expr))))

(define (clickhouse-emit-type ast)
  (match ast
    ((? string?)         ast)
    ((? number?)         (number->string ast))
    ((? symbol?)         (symbol->string ast))
    ((? clickhouse-sql-mapping?)    (clickhouse-emit-mapping ast))
    ((? clickhouse-sql-expression?) (clickhouse-emit-expression ast))

    ;; XXX: Nested is something special.
    ;; It receives a column definitions as clickhouse-sql-column.
    ((clickhouse-sql-type 'Nested arguments)
     (format "Nested(~a)" (clickhouse-emit-columns arguments)))
    ((clickhouse-sql-type name arguments)
     (string-append
      (symbol->string name)
      (if (not (null? arguments))
          (format "(~a)" (clickhouse-emit-types arguments))
          "")))))

(define (clickhouse-emit-types ast)
  (string-join (map clickhouse-emit-type ast) ", "))

(define (clickhouse-emit-tuple ast)
  (match ast
    ((clickhouse-sql-tuple arguments)
     (format "(~a)" (clickhouse-emit-expressions arguments)))))

(define (clickhouse-emit-mapping ast)
  (match ast
    ((clickhouse-sql-mapping key value)
     (string-append
      (clickhouse-emit-expression key)
      " = "
      (clickhouse-emit-expression value)))))

(define (clickhouse-emit-function ast)
  (match ast
    ((clickhouse-sql-function name arguments)
     (if (clickhouse-infix-operator? name)
         (string-join (map clickhouse-emit-expression arguments) (format " ~a " name))
         (format "~a(~a)" name (clickhouse-emit-expressions arguments))))))

(define (clickhouse-emit-expression ast)
  (match ast
    ((? clickhouse-sql-function?)  (clickhouse-emit-function ast))
    ((? clickhouse-sql-parameter?) (clickhouse-emit-parameter ast))
    ((? clickhouse-sql-tuple?)     (clickhouse-emit-tuple ast))

    ((clickhouse-sql-expression name value)
     (string-append
      (clickhouse-emit-expression value)
      (if name
          (string-append " AS " (clickhouse-emit-parameter name))
          "")))
    (_ (clickhouse-emit-value ast))))

(define (clickhouse-emit-expressions xs)
  (string-join (map clickhouse-emit-expression xs) ", "))

;;

(define (clickhouse-emit-column ast)
  (match ast
    ((clickhouse-sql-column name type)
     (format "~a ~a" name (clickhouse-emit-type type)))
    ((cons name type)
     (format "~a ~a" name (clickhouse-emit-type type)))))

(define (clickhouse-emit-columns ast)
  (string-join (map clickhouse-emit-column ast) ", "))

(define (clickhouse-emit-from ast)
  (match ast
    ((? string?)        (string-append "FROM " ast))
    ((? symbol?)        (clickhouse-emit-from (symbol->string ast)))
    ((? clickhouse-sql-parameter?) (clickhouse-emit-from (clickhouse-emit-parameter ast)))
    ((? clickhouse-sql-select?)    (clickhouse-emit-from (string-append "(" (clickhouse-emit-select ast) ")")))))

(define (clickhouse-emit-where ast)
  (match ast
    ((? string?)         (string-append "WHERE " ast))
    ((? clickhouse-sql-function?)   (clickhouse-emit-where (clickhouse-emit-function ast)))
    ((? clickhouse-sql-expression?) (clickhouse-emit-where (clickhouse-emit-expression ast)))))

(define (clickhouse-emit-cluster ast)
  (string-append "ON CLUSTER " (clickhouse-emit-parameter ast)))

(define (clickhouse-emit-engine ast)
  (string-append
   "ENGINE = "
   (match ast
     ((clickhouse-sql-engine name arguments)
      (string-append
       (symbol->string name)
       (if arguments
           (format "(~a)" (clickhouse-emit-expressions arguments))
           ""))))))

(define (clickhouse-emit-partition-by ast)
  (string-append "PARTITION BY " (clickhouse-emit-expression ast)))

(define (clickhouse-emit-order-by ast)
  (string-append "ORDER BY " (clickhouse-emit-expression ast)))

(define (clickhouse-emit-primary-key ast)
  (string-append "PRIMARY KEY " (clickhouse-emit-expression ast)))

(define (clickhouse-emit-sample-by ast)
  (string-append "SAMPLE BY " (clickhouse-emit-expression ast)))

(define (clickhouse-emit-create-table ast)
  (match ast
    ((clickhouse-sql-create-table name temporary? if-not-exists?
                                  cluster
                                  columns
                                  engine partition-by order-by primary-key sample-by)
     (string-append "CREATE " (if temporary? "TEMPORARY " "") "TABLE "
                    (if if-not-exists? "IF NOT EXISTS " "")
                    (clickhouse-emit-parameter name)
                    (if cluster (string-append " " (clickhouse-emit-cluster cluster)) "")
                    " (" (clickhouse-emit-columns columns) ") "
                    (clickhouse-emit-engine engine)
                    (if partition-by (string-append " " (clickhouse-emit-partition-by partition-by)) "")
                    (if order-by     (string-append " " (clickhouse-emit-order-by order-by))         "")
                    (if primary-key  (string-append " " (clickhouse-emit-primary-key primary-key))   "")
                    (if sample-by    (string-append " " (clickhouse-emit-sample-by sample-by))       "")))))

(define (clickhouse-emit-insert-columns keys)
  (string-append "(" (clickhouse-emit-expressions keys) ")"))

(define (clickhouse-emit-insert-row row)
  (format "(~a)" (clickhouse-emit-expressions row)))

(define (clickhouse-emit-insert-rows rows)
  (string-append
   "VALUES "
   (string-join
    (match rows
      ((? list?)   (map clickhouse-emit-insert-row rows))
      ((? vector?) (map clickhouse-emit-insert-row (vector->list rows))))
    ", ")))

(define (clickhouse-emit-insert ast)
  (match ast
    ((clickhouse-sql-insert name columns rows)
     (string-append "INSERT INTO " (clickhouse-emit-parameter name)
                    " " (if columns (string-append (clickhouse-emit-insert-columns columns) " ") "")
                    (clickhouse-emit-insert-rows rows)))))

(define (clickhouse-emit-select ast)
  (match ast
    ((clickhouse-sql-select expressions from where)
     (string-append "SELECT " (clickhouse-emit-expressions expressions)
                    (if from  (string-append " " (clickhouse-emit-from from)) "")
                    (if where (string-append " " (clickhouse-emit-where where)) "")))))
