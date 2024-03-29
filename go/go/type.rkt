#lang racket/base
(require racket/syntax
         syntax/parse
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))
(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(define-syntax (struct/keys stx)
  (syntax-parse stx
    ((_ t:id (xs:expr ...) rest ...)
     (with-syntax ((t-keys (format-id #'t "~a-keys" (syntax-e #'t))))
       (let ((keys (for/list ((x (in-list (attribute xs))))
                     (let ((key (syntax->datum x)))
                       (cond ((list? key) (car key))
                             (#t key))))))
         (quasisyntax (begin
                        (define t-keys (quote (unsyntax keys)))
                        (struct t (xs ...) rest ...))))))))

(struct/keys env     (ns package macro) #:transparent)
(struct/keys package (name (imports #:mutable)) #:transparent)

(struct transformer (name procs)  #:transparent)
(struct proc        (phase value) #:transparent)

;;

(struct walk:skip (unbox))

;;

(struct go:operator (id operands) #:prefab)

(struct go:package (name) #:prefab)

(struct go:imports (imports)         #:prefab)
(struct go:import  (package altname) #:prefab)

(struct go:var         (bindings)        #:prefab)
(struct go:var:binding (name type value) #:prefab)
(struct go:const       (bindings)        #:prefab)

(struct go:type                    (name value alias?) #:prefab)
(struct go:type:id                 (name type)         #:prefab)
(struct go:type:id:map             (key value)         #:prefab)
(struct go:type:id:struct          (fields)            #:prefab)
(struct go:type:id:struct:field    (name type tag)     #:prefab)
(struct go:type:id:interface       (fields)            #:prefab)
(struct go:type:id:interface:field (name type)         #:prefab)
(struct go:type:id:slice           (type)              #:prefab)
(struct go:type:id:array           (type size)         #:prefab)
(struct go:type:id:ptr             (type)              #:prefab)
(struct go:type:id:chan            (direction type)    #:prefab)
(struct go:type:id:func            (input output)      #:prefab)

(struct go:create (type value) #:prefab)

(struct go:def (id expr) #:prefab)
(struct go:set (id expr) #:prefab)

(struct go:go          (func)                #:prefab)
(struct go:if          (condition then else) #:prefab)

(struct go:alias        (namespace syms) #:prefab)
(struct go:alias:const  (sym)            #:prefab)
(struct go:alias:type   (sym)            #:prefab)

(struct go:for         (vars seq pred iter kind body) #:prefab)
(struct go:begin       (exprs) #:prefab)

(struct go:switch      (value cases)         #:prefab)
(struct go:select      (cases)               #:prefab)
(struct go:case        (predicate body)      #:prefab)

(struct go:cast        (value type)          #:prefab)
(struct go:cast:assert (type)                #:prefab)
(struct go:return      (values)              #:prefab)
(struct go:break       (label)               #:prefab)
(struct go:continue    (label)               #:prefab)
(struct go:spread      (expr)                #:prefab)
(struct go:label       (name body)           #:prefab)
(struct go:goto        (label)               #:prefab)
(struct go:iota        ()                    #:prefab)
(struct go:defer       (body)                #:prefab)
(struct go:slice       (value start end)     #:prefab)
(struct go:index       (value key)           #:prefab)
(struct go:key         (object key)          #:prefab)
(struct go:send        (chan value)          #:prefab)
(struct go:receive     (chan)                #:prefab)
(struct go:inc         (id)                  #:prefab)
(struct go:dec         (id)                  #:prefab)
(struct go:ref         (expr)                #:prefab)
(struct go:deref       (expr)                #:prefab)

(struct go:func               (name input output struct body) #:prefab)
(struct go:func:type:variadic (sym)                           #:prefab)
(struct go:func:call          (func arguments)                #:prefab)

(struct go:macro (name args expr) #:prefab)
(struct go:quote (expr)           #:prefab)

;;

(struct go:expr (exprs) #:prefab)
