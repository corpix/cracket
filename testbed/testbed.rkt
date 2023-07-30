#lang racket
(require ;; web-server/servlet
         ;; web-server/http
         ;; web-server/servlet-dispatch
         ;; web-server/web-server
         gregor
         threading
         racket/os
         racket/generic
         corpix/db
         corpix/strings
         corpix/struct
         (for-syntax corpix/syntax)
         "../http/http/http-router.rkt")

(define current-db (make-parameter #f))
(define current-runners (make-parameter (make-hash)))
(define task-states '(pending stopped started error))

(define-syntax (define-runner stx)
  (syntax-parse stx
    ((_ id:id (fields ...)
        (~seq #:executor executor-expr))
     (with-syntax ((constructor-sym (format-id #'id "make-~a" #'id))
                   (executor-sym (format-id #'id "~a-execute" #'id)))
       (syntax (begin
                 (define-struct id (fields ...) #:constructor-name constructor-sym)
                 (define executor-sym executor-expr)
                 (hash-set! (current-runners) (symbol->string 'id)
                            (make-hash (list (cons 'constructor constructor-sym)
                                             (cons 'executor executor-sym))))))))))

(define-runner runner-eval (code)
  #:executor (lambda (runner)
               (void (eval (runner-eval-code runner)))))

(define (runner-execute type . arguments)
  (let* ((runner-description (or (hash-ref (current-runners) type #f)
                                 (error (format "runner ~v is not defined" type))))
         (constructor (hash-ref runner-description 'constructor))
         (executor (hash-ref runner-description 'executor)))
    (executor (apply constructor arguments))))

;; (define-route (/foo request)
;;   #:method get
;;   #:path "/foo"
;;   (response/output (lambda (out)
;;                      (displayln "handling route" out))))

;; (define-route (/foo/bar/baz request)
;;   #:method get
;;   #:path "/foo/bar/baz"
;;   (response/xexpr '(html (head (title "test"))
;;                          (body (span "it is working")))))

;; (define stop
;;   (serve
;;    #:dispatch (dispatch/servlet http-dispatch-route)
;;    #:listen-ip "127.0.0.1"
;;    #:port 8000))

;;

(define-schema task
  ((id id/f #:primary-key #:auto-increment)
   (name string/f #:contract non-empty-string? #:unique)
   (description string/f)
   (runner-type string/f)
   (runner-arguments string/f)
   (created-at datetime/f)
   (updated-at datetime/f #:nullable)))

(define-schema task-instance
  ((id id/f #:primary-key #:auto-increment)
   (name string/f #:contract non-empty-string?)
   (description string/f)
   (task-id id/f #:foreign-key (task id))
   (state (enum/f task-states))
   (runner-type string/f)
   (runner-arguments string/f)
   (host string/f)
   (created-at datetime/f)
   (updated-at datetime/f #:nullable)))

(define-schema task-log
  ((id id/f #:primary-key #:auto-increment)
   (instance-id id/f #:foreign-key (task-instance id))
   (message string/f)
   (timestamp datetime/f)))

;;

(when (file-exists? "testbed.db")
    (delete-file "testbed.db"))
(current-db (sqlite3-connect
            #:database "testbed.db"
            #:mode 'create))

(create-all! (current-db))

;;

(void
 (when (not (lookup (current-db)
                    (~> (from task #:as t)
                        (where (= t.name ,"test")))))
   (insert! (current-db)
            (make-task #:name "test"
                       #:description "sample test task"
                       #:runner-type (symbol->string 'runner-eval)
                       #:runner-arguments (*->string '(displayln "hello world"))
                       #:created-at (now))))

 ;; reset any current host task states to "error" before starting
 (apply update! (current-db)
        (let ((query (~> (from task-instance #:as t)
                         (where (and (= t.host ,(gethostname))
                                     ;; fixme: index-of will not work for postgres
                                     ;; need to infer enum type in query constructor
                                     (or (= t.state ,(index-of task-states 'pending))
                                         (= t.state ,(index-of task-states 'started))))))))
          (for/list ((task-instance (in-entities (current-db) query)))
            (update-task-instance-state task-instance (lambda _ 'error))))))

;;


(define (task-instance-from-task task #:state (state 'pending))
  (make-task-instance #:name (task-name task)
                      #:description (task-description task)
                      #:task-id (task-id task)
                      #:state state
                      #:runner-type (task-runner-type task)
                      #:runner-arguments (task-runner-arguments task)
                      #:host (gethostname)
                      #:created-at (now)))

(define (task-list)
  (sequence->list
   (in-entities (current-db)
                (~> (from task #:as t)
                    (order-by ((t.updated-at) (t.created-at)))))))

(define (task-instance-create! task)
  (insert-one! (current-db) (task-instance-from-task task)))

(define (task-instance-state-set! instance state)
  (update-one! (current-db) (update-task-instance-state instance (thunk* state))))

(define (task-instance-run! instance)
  (let ((runner-type (task-instance-runner-type instance))
        (runner-arguments (string->* (task-instance-runner-arguments instance))))
    (runner-execute runner-type runner-arguments)))

(define (task-instance-list)
  (sequence->list
   (in-entities (current-db)
                (~> (from task-instance #:as t)
                    (order-by ((t.updated-at) (t.created-at)))))))



(define (task-log-append instance message)
  (insert-one! (current-db)
               (make-task-log #:instance-id (task-instance-id instance)
                              #:message message
                              #:timestamp (now))))

(define (task-log-list instance-id)
  (sequence->list
   (in-entities (current-db)
                (~> (from task-log #:as t)
                    (where (= t.instance-id ,instance-id))
                    (order-by ((t.timestamp)))))))

(define (task-run task)
  (let* ((instance (task-instance-create! task)))
    (task-log-append instance "starting")
    (with-handlers ((exn? (lambda (exn)
                            (task-log-append (task-instance-id instance) (exn-message exn))
                            (task-instance-state-set! instance 'error))))
      (task-instance-run! instance)
      (task-instance-state-set! instance 'stopped)
      (task-log-append instance "finished"))))

;; (task-list)
;; (task-instance-list)
(task-log-list 1)

(task-run (lookup (current-db)
                   (~> (from task #:as t)
                       (where (= t.name ,"test")))))
