#lang racket
(require syntax/parse
         "constant.rkt"
         "type.rkt")
(provide ClickhouseCreateTable
         ClickhouseInsert
         ClickhouseSelect
         ;;
         ClickhouseType
         ClickhouseFunction
         ClickhouseColumns
         ;; FIXME: we have function syntax class in export section
         ;; should we provide types and other "highlevel" entities too?
         ;; types - yes, engines - maybe, something other?
         )

(define-syntax-class ClickhouseUnquote
  (pattern ((~datum unquote) e:expr)))

(define-syntax-class ClickhouseParameter
  #:attributes (ast)
  (pattern v:id #:attr ast (clickhouse-sql-parameter #'v))
  (pattern v:string #:attr ast (clickhouse-sql-parameter #'v))
  (pattern v:number #:attr ast (clickhouse-sql-parameter #'v))
  (pattern v:ClickhouseUnquote #:attr ast (clickhouse-sql-parameter #'v)))

;;

(define-syntax-class ClickhousePrimitiveType
  #:description "primitive type"
  #:attributes (ast)
  (pattern id:id
           #:fail-when (and (not (clickhouse-primitive-type? (syntax->datum #'id))) #'id)
           "illegal type name"
           #:attr ast (clickhouse-sql-type #'id null)))

(define-syntax-class ClickhouseComplexTypeArgument
  #:description "complex type argument"
  #:attributes (ast)
  (pattern argument:ClickhouseType #:attr ast (attribute argument.ast))
  (pattern argument:ClickhouseColumn #:attr ast (attribute argument.ast))
  (pattern argument:ClickhouseMapping #:attr ast (attribute argument.ast))
  (pattern argument:ClickhouseExpression #:attr ast (attribute argument.ast)))

(define-syntax-class ClickhouseComplexType
  #:description "complex type"
  #:attributes (ast)
  (pattern (id:id arguments:ClickhouseComplexTypeArgument ...)
           #:fail-when (and (not (clickhouse-complex-type? (syntax->datum #'id)))
                            #'id)
           "expected type constructor"
           #:attr ast (clickhouse-sql-type #'id (attribute arguments.ast))))

(define-syntax-class ClickhouseType
  #:description "type"
  #:attributes (ast)
  (pattern type:ClickhousePrimitiveType #:attr ast (attribute type.ast))
  (pattern type:ClickhouseComplexType   #:attr ast (attribute type.ast)))

;;

(define-syntax-class ClickhouseMapping
  #:description "key with single value"
  #:attributes (ast)
  (pattern (key:ClickhouseExpression value:ClickhouseExpression)
           #:attr ast (clickhouse-sql-mapping (attribute key.ast)
                                              (attribute value.ast))))

(define-syntax-class ClickhouseColumn
  #:description "table column"
  #:attributes (ast)
  (pattern (name:id type:ClickhouseType)
           #:attr ast (clickhouse-sql-column #'name (attribute type.ast))))

(define-splicing-syntax-class ClickhouseColumns
  #:description "table columns list"
  #:attributes (ast)
  (pattern (~seq columns:ClickhouseColumn ...) #:attr ast (attribute columns.ast))
  (pattern v:ClickhouseUnquote #:attr ast #'v))

;;

(define-syntax-class ClickhouseExpressionInner
  #:attributes (ast)
  (pattern e:ClickhouseFunction #:attr ast (attribute e.ast))
  (pattern e:ClickhouseParameter #:attr ast (attribute e.ast))
  (pattern e:ClickhouseTuple #:attr ast (attribute e.ast)))

(define-syntax-class ClickhouseExpression
  #:attributes (ast)
  (pattern e:ClickhouseExpressionInner #:attr ast (clickhouse-sql-expression #f (attribute e.ast))))

(define-splicing-syntax-class ClickhouseMaybeNamedExpression
  #:attributes (ast)
  (pattern (~seq e:ClickhouseExpressionInner #:as name:ClickhouseParameter)
           #:attr ast (clickhouse-sql-expression (attribute name.ast) (attribute e.ast)))
  (pattern (~seq e:ClickhouseExpressionInner)
           #:attr ast (clickhouse-sql-expression #f (attribute e.ast))))

(define-splicing-syntax-class ClickhouseExpressions
  #:attributes (ast)
  (pattern (~seq e:ClickhouseExpression ...) #:attr ast (attribute e.ast)))

(define-splicing-syntax-class ClickhouseListOfExpressions
  #:attributes (ast)
  (pattern (~seq (e:ClickhouseExpressions) ...) #:attr ast (attribute e.ast)))

;;

(define-syntax-class ClickhouseFunction
  #:attributes (ast)
  (pattern (id:id arguments:ClickhouseExpression ...)
           #:fail-when (and (not (clickhouse-function? (syntax->datum #'id))) #'id) "expected function"
           #:attr ast (clickhouse-sql-function #'id (attribute arguments.ast))))

(define-syntax-class ClickhouseTuple
  #:attributes (ast)
  (pattern (e:ClickhouseExpression ...) #:attr ast (clickhouse-sql-tuple (attribute e.ast))))

;;

(define-syntax-class ClickhousePrimitiveEngine
  #:attributes (ast)
  (pattern engine:id
           #:fail-when (and (not (clickhouse-primitive-engine? (syntax->datum #'engine))) #'engine)
           "illegal engine name"
           #:attr ast (clickhouse-sql-engine #'engine #f)))

(define-syntax-class ClickhouseComplexEngine
  #:attributes (ast)
  (pattern (engine:id arguments:ClickhouseExpression ...)
           #:fail-when (and (not (clickhouse-complex-engine? (syntax->datum #'engine)))
                            #'engine)
           "illegal engine name"
           #:attr ast (clickhouse-sql-engine #'engine (attribute arguments.ast))))

(define-syntax-class ClickhouseEngine
  #:description "table engine"
  #:attributes (ast)
  (pattern engine:ClickhousePrimitiveEngine #:attr ast (attribute engine.ast))
  (pattern engine:ClickhouseComplexEngine   #:attr ast (attribute engine.ast)))

(define-splicing-syntax-class ClickhousePartitionBy
  #:description "partition expression"
  #:attributes (ast)
  (pattern (~seq #:partition-by e:ClickhouseExpression) #:attr ast (attribute e.ast))
  (pattern (~seq) #:attr ast #f))

(define-splicing-syntax-class ClickhouseOrderBy
  #:description "order expression"
  #:attributes (ast)
  (pattern (~seq #:order-by e:ClickhouseExpression) #:attr ast (attribute e.ast))
  (pattern (~seq) #:attr ast #f))

(define-splicing-syntax-class ClickhousePrimaryKey
  #:description "primary key expression"
  #:attributes (ast)
  (pattern (~seq #:primary-key e:ClickhouseExpression) #:attr ast (attribute e.ast))
  (pattern (~seq) #:attr ast #f))

(define-splicing-syntax-class ClickhouseSampleBy
  #:description "sample expression"
  #:attributes (ast)
  (pattern (~seq #:sample-by e:ClickhouseExpression) #:attr ast (attribute e.ast))
  (pattern (~seq) #:attr ast #f))

;;

(define-splicing-syntax-class ClickhouseFrom
  #:description "from clause"
  #:attributes (ast)
  (pattern (~seq #:from table:ClickhouseParameter) #:attr ast (attribute table.ast))
  (pattern (~seq #:from table:ClickhouseSelect) #:attr ast (attribute table.ast))
  (pattern (~seq) #:attr ast #f))

;;

(define-splicing-syntax-class ClickhouseWhere
  #:description "where clause"
  #:attributes (ast)
  (pattern (~seq #:where e:ClickhouseExpression) #:attr ast (attribute e.ast))
  (pattern (~seq) #:attr ast #f))

;;

(define-syntax-class ClickhouseTableName
  #:description "table name"
  #:attributes (ast)
  (pattern name:ClickhouseParameter #:attr ast (attribute name.ast)))

(define-splicing-syntax-class ClickhouseTableTemporary
  #:description "is temporary flag"
  #:attributes (ast)
  (pattern (~seq #:temporary) #:attr ast #t)
  (pattern (~seq) #:attr ast #f))

(define-splicing-syntax-class ClickhouseTableIfNotExists
  #:description "table if not exists"
  #:attributes (ast)
  (pattern (~seq #:if-not-exists) #:attr ast #t)
  (pattern (~seq) #:attr ast #f))

(define-splicing-syntax-class ClickhouseCluster
  #:description "cluster"
  #:attributes (ast)
  (pattern (~seq #:cluster cluster:ClickhouseParameter) #:attr ast (attribute cluster.ast))
  (pattern (~seq) #:attr ast #f))

;;

(define-syntax-class ClickhouseCreateTable
  #:description "create table"
  #:attributes (ast)
  (pattern (_ temporary:ClickhouseTableTemporary
              if-not-exists:ClickhouseTableIfNotExists
              name:ClickhouseTableName
              cluster:ClickhouseCluster
              #:columns columns:ClickhouseColumns
              #:engine engine:ClickhouseEngine
              partition-by:ClickhousePartitionBy
              order-by:ClickhouseOrderBy
              primary-key:ClickhousePrimaryKey
              sample-by:ClickhouseSampleBy)
           #:attr ast (clickhouse-sql-create-table
                       (attribute name.ast)
                       (attribute temporary.ast)
                       (attribute if-not-exists.ast)
                       (attribute cluster.ast)
                       (attribute columns.ast)
                       (attribute engine.ast)
                       (attribute partition-by.ast)
                       (attribute order-by.ast)
                       (attribute primary-key.ast)
                       (attribute sample-by.ast))))

(define-splicing-syntax-class ClickhouseInsertColumns
  #:description "column list"
  #:attributes (ast)
  (pattern (~seq #:columns (columns:ClickhouseExpressions)) #:attr ast (attribute columns.ast))
  (pattern (~seq) #:attr ast #f))

(define-splicing-syntax-class ClickhouseInsertRows
  #:description "row list"
  #:attributes (ast)
  (pattern (~seq #:rows unquote:ClickhouseUnquote) #:attr ast #'unquote)
  (pattern (~seq #:rows rows:ClickhouseListOfExpressions) #:attr ast (attribute rows.ast)))

(define-syntax-class ClickhouseInsert
  #:description "insert"
  #:attributes (ast)
  (pattern (_ #:into name:ClickhouseTableName
              columns:ClickhouseInsertColumns
              rows:ClickhouseInsertRows)
           #:attr ast (clickhouse-sql-insert
                       (attribute name.ast)
                       (attribute columns.ast)
                       (attribute rows.ast))))

(define-syntax-class ClickhouseSelect
  #:description "select"
  #:attributes (ast)
  (pattern (_ e:ClickhouseMaybeNamedExpression ... from:ClickhouseFrom where:ClickhouseWhere)
           #:attr ast (clickhouse-sql-select
                       (attribute e.ast)
                       (attribute from.ast)
                       (attribute where.ast))))
