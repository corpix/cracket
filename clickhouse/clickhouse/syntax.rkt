#lang racket/base

(require syntax/parse
         "constant.rkt"
         "type.rkt")

(provide CreateTable
         Insert
         Select
         ;;
         Type
         Function
         Columns
         ;; FIXME: we have function syntax class in export section
         ;; should we provide types and other "highlevel" entities too?
         ;; types - yes, engines - maybe, something other?

         #%app)

(define-syntax-class Unquote
  (pattern ((~datum unquote) e:expr)))

(define-syntax-class Parameter
  #:attributes (ast)
  (pattern v:id      #:attr ast (sql:parameter #'v))
  (pattern v:string  #:attr ast (sql:parameter #'v))
  (pattern v:number  #:attr ast (sql:parameter #'v))
  (pattern v:Unquote #:attr ast (sql:parameter #'v)))

;;

(define-syntax-class PrimitiveType
  #:description "primitive type"
  #:attributes (ast)
  (pattern id:id
           #:fail-when (and (not (primitive-type? (syntax->datum #'id))) #'id)
           "illegal type name"
           #:attr ast (sql:type #'id null)))

(define-syntax-class ComplexTypeArgument
  #:description "complex type argument"
  #:attributes (ast)
  (pattern argument:Type       #:attr ast (attribute argument.ast))
  (pattern argument:Column     #:attr ast (attribute argument.ast))
  (pattern argument:Mapping    #:attr ast (attribute argument.ast))
  (pattern argument:Expression #:attr ast (attribute argument.ast)))

(define-syntax-class ComplexType
  #:description "complex type"
  #:attributes (ast)
  (pattern (id:id arguments:ComplexTypeArgument ...)
           #:fail-when (and (not (complex-type? (syntax->datum #'id)))
                            #'id)
           "expected type constructor"
           #:attr ast (sql:type #'id (attribute arguments.ast))))

(define-syntax-class Type
  #:description "type"
  #:attributes (ast)
  (pattern type:PrimitiveType #:attr ast (attribute type.ast))
  (pattern type:ComplexType   #:attr ast (attribute type.ast)))

;;

(define-syntax-class Mapping
  #:description "key with single value"
  #:attributes (ast)
  (pattern (key:Expression value:Expression)
           #:attr ast (sql:mapping (attribute key.ast)
                                   (attribute value.ast))))

(define-syntax-class Column
  #:description "table column"
  #:attributes (ast)
  (pattern (name:id type:Type)
           #:attr ast (sql:column #'name (attribute type.ast))))

(define-splicing-syntax-class Columns
  #:description "table columns list"
  #:attributes (ast)
  (pattern (~seq columns:Column ...) #:attr ast (attribute columns.ast))
  (pattern v:Unquote                 #:attr ast #'v))

;;

(define-syntax-class ExpressionInner
  #:attributes (ast)
  (pattern e:Function   #:attr ast (attribute e.ast))
  (pattern e:Parameter  #:attr ast (attribute e.ast))
  (pattern e:Tuple      #:attr ast (attribute e.ast)))

(define-syntax-class Expression
  #:attributes (ast)
  (pattern e:ExpressionInner #:attr ast (sql:expression #f (attribute e.ast))))

(define-splicing-syntax-class MaybeNamedExpression
  #:attributes (ast)
  (pattern (~seq e:ExpressionInner #:as name:Parameter)
           #:attr ast (sql:expression (attribute name.ast) (attribute e.ast)))
  (pattern (~seq e:ExpressionInner)
           #:attr ast (sql:expression #f (attribute e.ast))))

(define-splicing-syntax-class Expressions
  #:attributes (ast)
  (pattern (~seq e:Expression ...) #:attr ast (attribute e.ast)))

(define-splicing-syntax-class ListOfExpressions
  #:attributes (ast)
  (pattern (~seq (e:Expressions) ...) #:attr ast (attribute e.ast)))

;;

(define-syntax-class Function
  #:attributes (ast)
  (pattern (id:id arguments:Expression ...)
           #:fail-when (and (not (function? (syntax->datum #'id))) #'id) "expected function"
           #:attr ast (sql:function #'id (attribute arguments.ast))))

(define-syntax-class Tuple
  #:attributes (ast)
  (pattern (e:Expression ...) #:attr ast (sql:tuple (attribute e.ast))))

;;

(define-syntax-class PrimitiveEngine
  #:attributes (ast)
  (pattern engine:id
           #:fail-when (and (not (primitive-engine? (syntax->datum #'engine)))
                            #'engine)
           "illegal engine name"
           #:attr ast (sql:engine #'engine #f)))

(define-syntax-class ComplexEngine
  #:attributes (ast)
  (pattern (engine:id arguments:Expression ...)
           #:fail-when (and (not (complex-engine? (syntax->datum #'engine)))
                            #'engine)
           "illegal engine name"
           #:attr ast (sql:engine #'engine (attribute arguments.ast))))

(define-syntax-class Engine
  #:description "table engine"
  #:attributes (ast)
  (pattern engine:PrimitiveEngine #:attr ast (attribute engine.ast))
  (pattern engine:ComplexEngine   #:attr ast (attribute engine.ast)))

(define-splicing-syntax-class PartitionBy
  #:description "partition expression"
  #:attributes (ast)
  (pattern (~seq #:partition-by e:Expression) #:attr ast (attribute e.ast))
  (pattern (~seq) #:attr ast #f))

(define-splicing-syntax-class OrderBy
  #:description "order expression"
  #:attributes (ast)
  (pattern (~seq #:order-by e:Expression) #:attr ast (attribute e.ast))
  (pattern (~seq) #:attr ast #f))

(define-splicing-syntax-class PrimaryKey
  #:description "primary key expression"
  #:attributes (ast)
  (pattern (~seq #:primary-key e:Expression) #:attr ast (attribute e.ast))
  (pattern (~seq) #:attr ast #f))

(define-splicing-syntax-class SampleBy
  #:description "sample expression"
  #:attributes (ast)
  (pattern (~seq #:sample-by e:Expression) #:attr ast (attribute e.ast))
  (pattern (~seq) #:attr ast #f))

;;

(define-splicing-syntax-class From
  #:description "from clause"
  #:attributes (ast)
  (pattern (~seq #:from table:Parameter) #:attr ast (attribute table.ast))
  (pattern (~seq #:from table:Select)    #:attr ast (attribute table.ast))
  (pattern (~seq) #:attr ast #f))

;;

(define-splicing-syntax-class Where
  #:description "where clause"
  #:attributes (ast)
  (pattern (~seq #:where e:Expression) #:attr ast (attribute e.ast))
  (pattern (~seq)                      #:attr ast #f))

;;

(define-syntax-class TableName
  #:description "table name"
  #:attributes (ast)
  (pattern name:Parameter #:attr ast (attribute name.ast)))

(define-splicing-syntax-class TableTemporary
  #:description "is temporary flag"
  #:attributes (ast)
  (pattern (~seq #:temporary) #:attr ast #t)
  (pattern (~seq) #:attr ast #f))

(define-splicing-syntax-class TableIfNotExists
  #:description "table if not exists"
  #:attributes (ast)
  (pattern (~seq #:if-not-exists) #:attr ast #t)
  (pattern (~seq) #:attr ast #f))

(define-splicing-syntax-class Cluster
  #:description "cluster"
  #:attributes (ast)
  (pattern (~seq #:cluster cluster:Parameter) #:attr ast (attribute cluster.ast))
  (pattern (~seq)                             #:attr ast #f))

;;

(define-syntax-class CreateTable
  #:description "create table"
  #:attributes (ast)
  (pattern (_ temporary:TableTemporary
              if-not-exists:TableIfNotExists
              name:TableName
              cluster:Cluster
              #:columns columns:Columns
              #:engine engine:Engine
              partition-by:PartitionBy
              order-by:OrderBy
              primary-key:PrimaryKey
              sample-by:SampleBy)
           #:attr ast (sql:create-table
                       (attribute name.ast) (attribute temporary.ast) (attribute if-not-exists.ast)
                       (attribute cluster.ast)
                       (attribute columns.ast)
                       (attribute engine.ast)
                       (attribute partition-by.ast) (attribute order-by.ast)
                       (attribute primary-key.ast) (attribute sample-by.ast))))

(define-splicing-syntax-class InsertColumns
  #:description "column list"
  #:attributes (ast)
  (pattern (~seq #:columns (columns:Expressions)) #:attr ast (attribute columns.ast))
  (pattern (~seq) #:attr ast #f))

(define-splicing-syntax-class InsertRows
  #:description "row list"
  #:attributes (ast)
  (pattern (~seq #:rows unquote:Unquote) #:attr ast #'unquote)
  (pattern (~seq #:rows rows:ListOfExpressions) #:attr ast (attribute rows.ast)))

(define-syntax-class Insert
  #:description "insert"
  #:attributes (ast)
  (pattern (_ #:into name:TableName
              columns:InsertColumns
              rows:InsertRows)
           #:attr ast (sql:insert
                       (attribute name.ast)
                       (attribute columns.ast)
                       (attribute rows.ast))))

(define-syntax-class Select
  #:description "select"
  #:attributes (ast)
  (pattern (_ e:MaybeNamedExpression ... from:From where:Where)
           #:attr ast (sql:select
                       (attribute e.ast)
                       (attribute from.ast)
                       (attribute where.ast))))
