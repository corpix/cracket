#lang racket
(provide (all-defined-out))

(define-struct (exn:fail:user:clickhouse:query exn:fail:user) (query status message))

(define-struct clickhouse-sql-type (name arguments) #:prefab)
(define-struct clickhouse-sql-mapping (key value) #:prefab)
(define-struct clickhouse-sql-column (name type) #:prefab)
(define-struct clickhouse-sql-engine (name arguments) #:prefab)
(define-struct clickhouse-sql-function (name arguments) #:prefab)
(define-struct clickhouse-sql-tuple (arguments) #:prefab)
(define-struct clickhouse-sql-expression (name value) #:prefab)
(define-struct clickhouse-sql-parameter (expr) #:prefab)
(define-struct clickhouse-sql-number (expr) #:prefab)

(define-struct clickhouse-sql-statement (value) #:prefab)
(define-struct clickhouse-sql-create-table
  (name temporary? if-not-exists?
        cluster
        columns
        engine partition-by order-by primary-key sample-by) #:prefab)
(define-struct clickhouse-sql-insert (name columns rows) #:prefab)
(define-struct clickhouse-sql-select (expression from where) #:prefab)
