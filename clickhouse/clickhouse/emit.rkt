#lang racket/base

(require racket/match
         racket/string
         racket/vector
         "constant.rkt"
         "escape.rkt"
         "type.rkt")

(provide emit-statement
         emit-create-table
         emit-insert
         emit-select
         ;;
         emit-type
         emit-function
         emit-columns)

(define (emit-statement ast)
  (match (sql:statement-value ast)
    ((? sql:create-table? ast) (emit-create-table ast))
    ((? sql:insert?       ast) (emit-insert ast))
    ((? sql:select?       ast) (emit-select ast))))

(define (emit-value ast)
  (match ast
    ((? number?) (number->string ast))
    ((? symbol?) (sql-escape (symbol->string ast)))
    ((? string?) (format "'~a'" (sql-escape ast)))))

(define (emit-parameter ast)
  (match ast
    ((sql:parameter expr) (emit-value expr))))

(define (emit-type ast)
  (match ast
    ((? string?)         ast)
    ((? number?)         (number->string ast))
    ((? symbol?)         (symbol->string ast))
    ((? sql:mapping?)    (emit-mapping ast))
    ((? sql:expression?) (emit-expression ast))

    ;; XXX: Nested is something special.
    ;; It receives a column definitions as sql:column.
    ((sql:type 'Nested arguments)
     (format "Nested(~a)" (emit-columns arguments)))
    ((sql:type name arguments)
     (string-append
      (symbol->string name)
      (if (not (null? arguments))
          (format "(~a)" (emit-types arguments))
          "")))))

(define (emit-types ast)
  (string-join (map emit-type ast) ", "))

(define (emit-tuple ast)
  (match ast
    ((sql:tuple arguments)
     (format "(~a)" (emit-expressions arguments)))))

(define (emit-mapping ast)
  (match ast
    ((sql:mapping key value)
     (string-append
      (emit-expression key)
      " = "
      (emit-expression value)))))

(define (emit-function ast)
  (match ast
    ((sql:function name arguments)
     (if (infix-operator? name)
         (string-join (map emit-expression arguments) (format " ~a " name))
         (format "~a(~a)" name (emit-expressions arguments))))))

(define (emit-expression ast)
  (match ast
    ((? sql:function?)  (emit-function ast))
    ((? sql:parameter?) (emit-parameter ast))
    ((? sql:tuple?)     (emit-tuple ast))

    ((sql:expression name value)
     (string-append
      (emit-expression value)
      (if name
          (string-append " AS " (emit-parameter name))
          "")))
    (_ (emit-value ast))))

(define (emit-expressions xs)
  (string-join (map emit-expression xs) ", "))

;;

(define (emit-column ast)
  (match ast
    ((sql:column name type)
     (format "~a ~a" name (emit-type type)))
    ((cons name type)
     (format "~a ~a" name (emit-type type)))))

(define (emit-columns ast)
  (string-join (map emit-column ast) ", "))

(define (emit-from ast)
  (match ast
    ((? string?)        (string-append "FROM " ast))
    ((? symbol?)        (emit-from (symbol->string ast)))
    ((? sql:parameter?) (emit-from (emit-parameter ast)))
    ((? sql:select?)    (emit-from (string-append "(" (emit-select ast) ")")))))

(define (emit-where ast)
  (match ast
    ((? string?)         (string-append "WHERE " ast))
    ((? sql:function?)   (emit-where (emit-function ast)))
    ((? sql:expression?) (emit-where (emit-expression ast)))))

(define (emit-cluster ast)
  (string-append "ON CLUSTER " (emit-parameter ast)))

(define (emit-engine ast)
  (string-append
   "ENGINE = "
   (match ast
     ((sql:engine name arguments)
      (string-append
       (symbol->string name)
       (if arguments
           (format "(~a)" (emit-expressions arguments))
           ""))))))

(define (emit-partition-by ast)
  (string-append "PARTITION BY " (emit-expression ast)))

(define (emit-order-by ast)
  (string-append "ORDER BY " (emit-expression ast)))

(define (emit-primary-key ast)
  (string-append "PRIMARY KEY " (emit-expression ast)))

(define (emit-sample-by ast)
  (string-append "SAMPLE BY " (emit-expression ast)))

(define (emit-create-table ast)
  (match ast
    ((sql:create-table name temporary? if-not-exists?
                       cluster
                       columns
                       engine partition-by order-by primary-key sample-by)
     (string-append "CREATE " (if temporary? "TEMPORARY " "") "TABLE "
                    (if if-not-exists? "IF NOT EXISTS " "")
                    (emit-parameter name)
                    (if cluster (string-append " " (emit-cluster cluster)) "")
                    " (" (emit-columns columns) ") "
                    (emit-engine engine)
                    (if partition-by (string-append " " (emit-partition-by partition-by)) "")
                    (if order-by     (string-append " " (emit-order-by order-by))         "")
                    (if primary-key  (string-append " " (emit-primary-key primary-key))   "")
                    (if sample-by    (string-append " " (emit-sample-by sample-by))       "")))))

(define (emit-insert-columns keys)
  (string-append "(" (emit-expressions keys) ")"))

(define (emit-insert-row row)
  (format "(~a)" (emit-expressions row)))

(define (emit-insert-rows rows)
  (string-append
   "VALUES "
   (string-join
    (match rows
      ((? list?)   (map emit-insert-row rows))
      ((? vector?) (map emit-insert-row (vector->list rows))))
    ", ")))

(define (emit-insert ast)
  (match ast
    ((sql:insert name columns rows)
     (string-append "INSERT INTO " (emit-parameter name)
                    " " (if columns (string-append (emit-insert-columns columns) " ") "")
                    (emit-insert-rows rows)))))

(define (emit-select ast)
  (match ast
    ((sql:select expressions from where)
     (string-append "SELECT " (emit-expressions expressions)
                    (if from  (string-append " " (emit-from from)) "")
                    (if where (string-append " " (emit-where where)) "")))))
