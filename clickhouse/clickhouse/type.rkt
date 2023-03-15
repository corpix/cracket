#lang racket
(provide (all-defined-out))

(struct exn:fail:user:clickhouse:query exn:fail:user (query status message))

(struct clickhouse-sql-type (name arguments) #:prefab)
(struct clickhouse-sql-mapping (key value) #:prefab)
(struct clickhouse-sql-column (name type) #:prefab)
(struct clickhouse-sql-engine (name arguments) #:prefab)
(struct clickhouse-sql-function (name arguments) #:prefab)
(struct clickhouse-sql-tuple (arguments) #:prefab)
(struct clickhouse-sql-expression (name value) #:prefab)
(struct clickhouse-sql-parameter (expr) #:prefab)
(struct clickhouse-sql-number (expr) #:prefab)

(struct clickhouse-sql-statement (value) #:prefab)
(struct clickhouse-sql-create-table (name temporary? if-not-exists?
                                          cluster
                                          columns
                                          engine partition-by order-by primary-key sample-by) #:prefab)
(struct clickhouse-sql-insert (name columns rows) #:prefab)
(struct clickhouse-sql-select (expression from where) #:prefab)
