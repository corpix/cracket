#lang racket
(provide (all-defined-out))

(struct exn:fail:user:clickhouse:query exn:fail:user (query status message))

(struct sql:type (name arguments) #:prefab)
(struct sql:mapping (key value) #:prefab)
(struct sql:column (name type) #:prefab)
(struct sql:engine (name arguments) #:prefab)
(struct sql:function (name arguments) #:prefab)
(struct sql:tuple (arguments) #:prefab)
(struct sql:expression (name value) #:prefab)
(struct sql:parameter (expr) #:prefab)
(struct sql:number (expr) #:prefab)

(struct sql:statement (value) #:prefab)
(struct sql:create-table (name temporary? if-not-exists?
                               cluster
                               columns
                               engine partition-by order-by primary-key sample-by) #:prefab)
(struct sql:insert (name columns rows) #:prefab)
(struct sql:select (expression from where) #:prefab)
