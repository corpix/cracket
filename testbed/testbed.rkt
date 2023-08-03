#lang racket
(require web-server/servlet
         web-server/http
         web-server/http/response
         web-server/servlet-dispatch
         web-server/web-server
         web-server/dispatchers/filesystem-map
         (prefix-in files: web-server/dispatchers/dispatch-files)
         (prefix-in filter: web-server/dispatchers/dispatch-filter)
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         net/url
         net/mime-type
         gregor
         threading
         racket/os
         racket/generic
         racket/pretty
         racket/system
         racket/match
         racket/string
         racket/function
         corpix/db
         corpix/css
         corpix/strings
         corpix/struct
         corpix/path
         (for-syntax corpix/syntax)
         "../http/http/http-router.rkt"
         "../http/http/http-status.rkt")

(define current-db (make-parameter #f))
(define current-runners (make-parameter (make-hash)))
(define current-resources (make-parameter (make-hash)))
(define current-tcp-range (make-parameter (cons 45000 46000)))
(define current-css (make-parameter (box null)))
(define task-states '(pending running exited errored killed))
(define task-states-hash (for/hash ((state (in-list task-states)) (index (in-naturals))) (values state index)))
(define task-states-running '(pending running))

;;

(define (css-append! css-expr)
  (set-box! (current-css)
            (append (unbox (current-css))
                    (list css-expr))))

(define (make-css)
  (let* ((expr (css-expr (body #:margin (40px auto)
                               #:line-height 1.6
                               #:font-size 16px
                               #:font-family "Fira Code"
                               #:color |#444|
                               #:padding (0 10px))
                         (h1 h2 h3 #:line-height 1.2)
                         (ul #:list-style none)
                         (.navigation #:display flex
                                      #:align-items center
                                      #:justify-content center)
                         ((.navigation a:before) (.navigation a:after)
                                                 #:content ""
                                                 #:position absolute
                                                 #:transition |transform .5s ease|)
                         ((.navigation a:before) #:left 0
                                                 #:bottom 0
                                                 #:width 100%
                                                 #:height 2px
                                                 #:background |#000|
                                                 #:transform |scaleX(0)|)
                         ((.navigation a:hover:before) #:transform |scaleX(1)|)
                         ((.navigation a) #:width auto
                                          #:height 50px
                                          #:position relative
                                          #:text-decoration none
                                          #:line-height 35px
                                          #:background-color transparent
                                          #:color |#000|
                                          #:padding (10px 0 0 0)
                                          #:margin (0 10px)
                                          #:font-size 18px
                                          #:font-weight bold
                                          #:text-align center
                                          #:text-transform uppercase
                                          #:box-sizing border-box)
                         (.content #:padding-top 15px)
                         (pre #:overflow auto
                              #:padding 20px)

                         (a.task-instance
                          #:position relative
                          #:padding-right 10px
                          #:text-decoration none)

                         (a.task-instance.state-pending::after
                          #:content ""
                          #:position absolute
                          #:top 50%
                          #:right 0
                          #:transform |translateY(-50%)|
                          #:width 0
                          #:height 0
                          #:border-style solid
                          #:border-width (6px 0 6px 6px)
                          #:border-color |transparent transparent transparent grey|)
                         (a.task-instance.state-exited::after
                          #:content ""
                          #:position absolute
                          #:top 50%
                          #:right 0
                          #:transform |translateY(-50%)|
                          #:width 0
                          #:height 0
                          #:border-style solid
                          #:border-width (6px 0 6px 6px)
                          #:border-color |transparent transparent transparent green|)
                         (a.task-instance.state-running::after
                          #:content ""
                          #:position absolute
                          #:top 50%
                          #:right 0
                          #:transform |translateY(-50%)|
                          #:width 0
                          #:height 0
                          #:border-style solid
                          #:border-width (6px 0 6px 6px)
                          #:border-color |transparent transparent transparent #00cfff|)
                         (a.task-instance.state-errored::after
                          #:content ""
                          #:position absolute
                          #:top 50%
                          #:right 0
                          #:transform |translateY(-50%)|
                          #:width 0
                          #:height 0
                          #:border-style solid
                          #:border-width (6px 0 6px 6px)
                          #:border-color |transparent transparent transparent red|)
                         (a.task-instance.state-killed::after
                          #:content ""
                          #:position absolute
                          #:top 50%
                          #:right 0
                          #:transform |translateY(-50%)|
                          #:width 0
                          #:height 0
                          #:border-style solid
                          #:border-width (6px 0 6px 6px)
                          #:border-color |transparent transparent transparent pink|)))
         (expr (append (list expr) (unbox (current-css)))))
    (string-join (map css-expr->css expr) "")))

(define static-dispatcher
  (let ((url->path/static (make-url->path "static")))
    (files:make #:url->path (lambda (u)
                              (url->path/static
                               (struct-copy url u [path (cdr (url-path u))])))
                #:path->mime-type path-mime-type)))

;;

(define-struct runner-job (task-instance thread) #:transparent)
(define-struct runner-jobs (semaphore store) #:transparent)

(define current-runner-jobs (make-parameter
                             (make-runner-jobs (make-semaphore 1)
                                               (make-hash))))

(define (runner-job-register! id job #:registry (jobs (current-runner-jobs)))
  (call-with-semaphore
   (runner-jobs-semaphore jobs)
   (thunk (hash-set! (runner-jobs-store jobs) id job))))

(define (runner-job-unregister! id #:registry (jobs (current-runner-jobs)))
  (call-with-semaphore
   (runner-jobs-semaphore jobs)
   (thunk (hash-remove! (runner-jobs-store jobs) id))))

(define (runner-job-ref id #:registry (jobs (current-runner-jobs)))
  (call-with-semaphore
   (runner-jobs-semaphore jobs)
   (thunk (hash-ref (runner-jobs-store jobs) id #f))))

;;

(define-syntax (define-runner stx)
  (syntax-parse stx
    ((_ (id:id instance-id:id (fields ...))
        (~seq #:executor executor-expr))
     (with-syntax* ((struct-name (format-id #'id "runner-~a" #'id))
                    (constructor-sym (format-id #'id "make-~a" #'struct-name))
                    (executor-sym (format-id #'id "~a-execute" #'struct-name)))
       (syntax (begin
                 (define-struct struct-name (fields ...) #:constructor-name constructor-sym)
                 (define (executor-sym instance-id) executor-expr)
                 (hash-set! (current-runners) 'id
                            (make-hash (list (cons 'constructor constructor-sym)
                                             (cons 'executor executor-sym))))))))))

(define-runner (eval runner (code))
  #:executor (void (eval (runner-eval-code runner))))

(define-runner (shell runner (command arguments))
  #:executor (match-let* ((command (runner-shell-command runner))
                          (arguments (runner-shell-arguments runner))
                          (executable (find-executable-path command))
                          ((list out in pid err control)
                           (begin
                             (displayln (format "running: ~a ~a" executable arguments))
                             (apply process* executable arguments)))
                          (io (for/list ((port (list out err)))
                                (thread (thunk
                                         (for ((line (in-lines port)))
                                           (displayln line)))))))
               (close-output-port in)
               (control 'wait)
               (dynamic-wind
                 void
                 (thunk (let ((code (control 'exit-code)))
                          (when (not (= code 0))
                            (raise (let ((line (string-join (append (list command) arguments) " "))
                                         (stderr (or (port-closed? err)
                                                     (string-trim (port->string err) "\n"
                                                                  #:left? #t
                                                                  #:right? #t))))
                                     (make-exn:fail:user
                                      (format "shell command '~s' exited with code ~a~a"
                                              line code (if stderr (format ", err: ~a" stderr) ""))
                                      (current-continuation-marks)))))))
                 (thunk (for ((port (in-list io)))
                          (sync port))
                        (close-input-port out)
                        (close-input-port err)))))

(define (runner-descriptor type)
  (or (hash-ref (current-runners) type #f)
      (error (format "runner ~v is not defined" type))))

(define (runner-execute type arguments)
  (let* ((descriptor (runner-descriptor type))
         (construct (hash-ref descriptor 'constructor))
         (execute (hash-ref descriptor 'executor)))
    (execute (apply construct arguments))))

;;

(define-syntax (define-resource stx)
  (syntax-parse stx
    ((_ (id:id resource-instance-id:id (fields ...))
        (~seq #:prelude prelude-expr)
        (~seq #:allocator allocator-expr)
        (~seq #:representor representor-expr))
     (with-syntax* ((struct-name (format-id #'id "resource-~a" #'id))
                    (constructor-sym (format-id #'id "make-~a" #'struct-name))
                    (allocator-sym (format-id #'id "~a-allocator" #'struct-name))
                    (representor-sym (format-id #'id "~a-representor" #'struct-name)))
       (syntax (begin
                 (define-struct struct-name (fields ...)
                   #:constructor-name constructor-sym
                   #:transparent)
                 (define (allocator-sym) allocator-expr)
                 (define (representor-sym resource-instance-id) representor-expr)
                 (void prelude-expr)
                 (hash-set! (current-resources) 'id
                            (make-hash (list (cons 'constructor constructor-sym)
                                             (cons 'allocator allocator-sym)
                                             (cons 'representor representor-sym))))))))))

(define-resource (vnc resource (port))
  #:prelude
  (css-append! (css-expr (.vnc-representor #:width 100% #:min-height 780px)))
  #:allocator
  (call-with-transaction
   (current-db)
   (thunk
    (let* ((range (current-tcp-range))
           (next (thunk (random (car range) (cdr range))))
           (query (~> (from resource-instance #:as r)
                      (join task-instance #:as t #:on (= r.task-instance-id t.id))
                      (where (and (= t.host ,(gethostname))
                                  (= r.type "vnc")
                                  (or (= t.state ,(hash-ref task-states-hash 'pending))
                                      (= t.state ,(hash-ref task-states-hash 'running))))))))
      (make-resource-vnc
       (let loop ((port (next)))
         (for/fold ((port port))
                   ((resource-instance (in-entities (current-db) query)))
           (let ((record (assoc port (resource-instance-data))))
             (if (and record (= (cdr record) port))
                 (loop (next)) port))))))))
  #:representor
  `(iframe ((class "vnc-representor")
            (src ,(format "/static/vnc/?path=resources/vnc/~a" (resource-vnc-port resource))))))

(define (resource-descriptor type)
  (or (hash-ref (current-resources) type #f)
      (error (format "resource ~v is not defined" type))))

(define (resource-allocate type)
  (let* ((descriptor (resource-descriptor type))
         (allocate (hash-ref descriptor 'allocator)))
    (allocate)))

(define (resource-represent type data)
  (let* ((descriptor (resource-descriptor type))
         (construct (hash-ref descriptor 'constructor))
         (represent (hash-ref descriptor 'representor)))
    (represent (apply construct data))))

;;

(define-schema task
  ((id id/f #:primary-key #:auto-increment)
   (name string/f #:contract non-empty-string? #:unique)
   (description string/f #:nullable)
   (runner-type symbol/f)
   (runner-script sexpr/f)
   (created-at datetime/f)
   (updated-at datetime/f #:nullable)))

(define-schema task-instance
  ((id id/f #:primary-key #:auto-increment)
   (name string/f #:contract non-empty-string?)
   (description string/f #:nullable)
   (task-id id/f #:foreign-key (task id) #:nullable)
   (state (enum/f task-states))
   (runner-type symbol/f)
   (runner-script sexpr/f)
   (host string/f)
   (created-at datetime/f)
   (updated-at datetime/f #:nullable)))

(define-schema task-instance-log
  ((id id/f #:primary-key #:auto-increment)
   (task-instance-id id/f #:foreign-key (task-instance id) #:nullable)
   (message string/f)
   (timestamp datetime/f)))

(define-schema resource
  ((id id/f #:primary-key #:auto-increment)
   (task-id id/f #:foreign-key (task id) #:nullable)
   (type symbol/f)))

(define-schema resource-instance
  ((id id/f #:primary-key #:auto-increment)
   (resource-id id/f #:foreign-key (resource id) #:nullable)
   (task-instance-id id/f #:foreign-key (task-instance id) #:nullable)
   (type symbol/f)
   (data sexpr/f)))

;;


(define (task->task-instance task #:state (state 'pending))
  (make-task-instance #:name (task-name task)
                      #:description (task-description task)
                      #:task-id (task-id task)
                      #:state state
                      #:runner-type (task-runner-type task)
                      #:runner-script (task-runner-script task)
                      #:host (gethostname)
                      #:created-at (now)))

(define (resource->resource-instance resource
                                     #:task-instance-id task-instance-id
                                     #:data (data null))
  (make-resource-instance #:resource-id (resource-id resource)
                          #:task-instance-id task-instance-id
                          #:type (resource-type resource)
                          #:data data))

;;

(define (resources-create! resources #:task task)
  (let ((task-id (task-id task)))
    (apply insert! (current-db)
           (for/list ((resource (in-list resources)))
             (update-resource-task-id resource (thunk* task-id))))))

(define (resource-instances-create! resources task-instance)
  (call-with-transaction
   (current-db)
   (thunk (apply insert! (current-db)
                 (for/list ((resource resources))
                   (resource->resource-instance
                    resource
                    #:task-instance-id (task-instance-id task-instance)
                    #:data (struct-data (resource-allocate (resource-type resource)))))))))

(define (task-instance-create! task)
  (call-with-transaction
   (current-db)
   (thunk
    (let ((task-instance (insert-one! (current-db) (task->task-instance task)))
          (resources (in-entities (current-db)
                                  (~> (from resource #:as c)
                                      (where (= c.task-id ,(task-id task)))))))
      (begin0 task-instance
        (resource-instances-create! resources task-instance))))))

(define (task-instance-state-set! instance state)
  (update-one! (current-db) (~> instance
                                (update-task-instance-state _ (thunk* state))
                                (update-task-instance-updated-at _ (thunk* (now)))))
  (task-instance-log-append! instance (symbol->string state)))

(define (task-instance-log-append! instance message)
  (insert-one! (current-db)
               (make-task-instance-log #:task-instance-id (task-instance-id instance)
                                       #:message message
                                       #:timestamp (now))))

(define (task-create! task #:resources (resources null))
  (call-with-transaction
   (current-db)
   (thunk
    (let ((task (insert-one! (current-db) task)))
      (begin0 task
        (resources-create! resources #:task task))))))

;;

(define (task-list)
  (sequence->list
   (in-entities (current-db)
                (~> (from task #:as t)
                    (order-by ((t.updated-at) (t.created-at)))))))

(define (task-get id)
  (lookup (current-db) (~> (from task #:as t)
                           (where (= t.id ,id)))))

(define (task-instance-get id)
  (lookup (current-db)
          (~> (from task-instance #:as t)
              (where (= t.id ,id)))))

(define (task-instance-list (task #f))
  (let* ((query (cond (task (~> (from task-instance #:as t)
                                (where (= t.task-id ,(task-id task)))
                                (order-by ((t.updated-at #:desc) (t.created-at #:desc)))))
                      (else (~> (from task-instance #:as t)
                                (order-by ((t.updated-at #:desc) (t.created-at #:desc)))))))
         (sequence (in-entities (current-db) query)))
    (sequence->list sequence)))

(define (task-instance-log-list task-instance)
  (let* ((query (~> (from task-instance-log #:as t)
                    (where (= t.task-instance-id ,(task-instance-id task-instance)))
                    (order-by ((t.timestamp)))))
         (sequence (in-entities (current-db) query)))
    (sequence->list sequence)))

(define (task-instance-resources-list task-instance)
  (let* ((query (~> (from resource-instance #:as i)
                    (where (= i.task-instance-id ,(task-instance-id task-instance)))
                    (order-by ((i.id)))))
         (sequence (in-entities (current-db) query)))
    (sequence->list sequence)))

;;

(define (task-instance-run! instance)
  (let* ((runner-type (task-instance-runner-type instance))
         (runner-script (task-instance-runner-script instance))
         (job-thunk (thunk
                     (task-instance-state-set! instance 'running)
                     (let*-values (((custodian) (make-custodian))
                                   ((cancel-evt) (handle-evt (thread-receive-evt) (thunk* (thread-receive))))
                                   ((in out) (make-pipe))
                                   ((io-pump) (thread (thunk
                                                       (for ((line (in-lines in)))
                                                         (task-instance-log-append! instance line)))))
                                   ((worker-exn) #f)
                                   ((worker-job-thread)
                                    (parameterize ((current-output-port out)
                                                   (current-error-port out))
                                      (thread (thunk
                                               (parameterize ((current-custodian custodian)
                                                              (current-subprocess-custodian-mode 'kill)
                                                              (subprocess-group-enabled #t))
                                                 (with-handlers ((exn? (lambda (exn) (set! worker-exn exn))))
                                                   (runner-execute runner-type runner-script)))))))
                                   ((result)
                                    (dynamic-wind
                                      void
                                      (thunk (sync worker-job-thread cancel-evt))
                                      (thunk (custodian-shutdown-all custodian)
                                             (close-output-port out)
                                             (sync io-pump)
                                             (close-input-port in)))))
                       (cond
                         ((eq? result 'kill)
                          (task-instance-state-set! instance 'killed))
                         (worker-exn
                          (task-instance-log-append! instance (exn-message worker-exn))
                          (task-instance-state-set! instance 'errored))
                         ((thread? result)
                          (task-instance-state-set! instance 'exited)))))))
    (runner-job-register!
     (task-instance-id instance)
     (make-runner-job instance (thread job-thunk)))))

(define (task-instance-kill! instance)
  (let* ((id (task-instance-id instance))
         (job (runner-job-ref id)))
    (unless job
      (error (format "job ~a is not running" id)))
    (thread-send (runner-job-thread job) 'kill)
    (sync (runner-job-thread job))))

(define (task-run! task)
  (let* ((task-instance (task-instance-create! task)))
    (task-instance-run! task-instance)))

;;

(define (message-task-not-found id)
  (format "task ~a was not found" id))

(define (message-task-instance-not-found id)
  (format "task instance ~a was not found" id))

;;

(define (xexpr-navigation)
  `(nav ((class "navigation"))
        (a ((href "/tasks")) "Tasks")
        (a ((href "/tasks/instances")) "Instances")))

(define (xexpr-page body
                    #:title (title "")
                    #:css (css (make-css)))
  `(html (head (title ,title)
               (style ,css))
         (body (div ((class "root"))
                    (div ((class "container"))
                         (div ((class="header"))
                              ,(xexpr-navigation))
                         (div ((class "content"))
                              ,body))))))

(define (xexpr-task-instances task-instances)
  `(div (ul ,@(for/list ((task-instance (in-list task-instances)))
                `(li (a ((href ,(format "/tasks/instances/~a" (task-instance-id task-instance)))
                         (class ,(format "task-instance state-~a" (task-instance-state task-instance))))
                        ,(task-instance-name task-instance)))))))

(define (xexpr-task-instance-resources resources)
  `(ul ,@(for/list ((resource (in-list resources)))
           `(li ,(resource-represent (resource-instance-type resource)
                                     (resource-instance-data resource))))))

;;

(define (response-task-not-found id)
  (response/xexpr (message-task-not-found id)
                  #:code http-status-not-found))

(define (response-task-instance-not-found id)
  (response/xexpr (message-task-instance-not-found id)
                  #:code http-status-not-found))

;;

(define-route (/ request)
  #:method get
  (response/xexpr (xexpr-page "")))

(define-route (/tasks request)
  #:method get
  (response/xexpr
   (xexpr-page
    `(div (ul ,@(for/list ((task (in-list (task-list))))
                  `(li (a ((href ,(format "/tasks/~a" (task-id task))))
                          ,(task-name task))))))
    #:title "tasks")))

(define-route (/tasks/:task-id/instantiate request)
  #:method post
  (let ((id (http-route-parameter ':task-id)))
    (if (task-run! (task-get id))
        (redirect-to (format "/tasks/~a" id))
        (response-task-not-found id))))

(define-route (/tasks/:task-id request)
  #:method get
  (let* ((id (http-route-parameter ':task-id))
         (task (task-get id)))
    (response/xexpr
     #:code (if task http-status-ok http-status-not-found)
     (xexpr-page
      (if task
          `(div (table (tr (td "name") (td (a ((href ,(format "/tasks/~a" (task-id task))))
                                              ,(task-name task))))
                       (tr (td "runner") (td ,(format "~a" (task-runner-type task))))
                       (tr (td "created-at") (td ,(datetime->iso8601 (task-created-at task))))
                       (tr (td "updated-at")
                           (td ,(let ((updated-at (task-updated-at task)))
                                  (if (sql-null? updated-at) "" (datetime->iso8601 updated-at))))))

                (form ((method "post")
                       (action ,(format "/tasks/~a/instantiate" (task-id task))))
                      (button ((type "submit")) "instantiate"))

                (ul (li (details
                         (summary (b "runner"))
                         (div (pre ,(pretty-format (task-runner-script task))))))
                    ;; (li (details
                    ;;      (summary (b "resources"))
                    ;;      (div (pre ,(pretty-format (task-resources task))))))

                    (li ,(let ((task-instances (task-instance-list task)))
                           `(details (,@(if (findf (lambda (task-instance)
                                                     (memq (task-instance-state task-instance)
                                                           task-states-running))
                                                   task-instances)
                                            '((open "")) null))
                                     (summary (b ,(format "instances (~a)" (length task-instances))))
                                     ,(xexpr-task-instances task-instances))))))
          (message-task-not-found id))
      #:title "task"))))

(define-route (/tasks/instances request)
  #:method get
  (response/xexpr
   (xexpr-page (xexpr-task-instances (task-instance-list)) #:title "task instances")))

(define-route (/tasks/instances/:task-instance-id request)
  #:method get
  (let* ((id (http-route-parameter ':task-instance-id))
         (task-instance (task-instance-get id)))
    (response/xexpr
     #:code (if task-instance http-status-ok http-status-not-found)
     (xexpr-page
      (if task-instance
          (let ((logs (task-instance-log-list task-instance))
                (resources (task-instance-resources-list task-instance)))
            `(div (table (tr (td "name") (td (a ((href ,(format "/tasks/~a" (task-instance-task-id task-instance))))
                                                ,(task-instance-name task-instance))))
                         (tr (td "state") (td ,(format "~a" (task-instance-state task-instance))))
                         (tr (td "runner") (td ,(format "~a" (task-instance-runner-type task-instance))))
                         (tr (td "host") (td ,(task-instance-host task-instance)))
                         (tr (td "created-at") (td ,(datetime->iso8601 (task-instance-created-at task-instance))))
                         (tr (td "updated-at")
                             (td ,(let ((updated-at (task-instance-updated-at task-instance)))
                                    (if (sql-null? updated-at) "" (datetime->iso8601 updated-at))))))

                  (form ((method "post")
                         (action ,(format "/tasks/instances/~a/kill" (task-instance-id task-instance))))
                        (button ((type "submit")
                                 ,@(if (memq (task-instance-state task-instance) task-states-running)
                                       null
                                       '((disabled ""))))
                                "kill"))

                  (ul (li (details
                           (summary (b "runner"))
                           (div (pre ,(pretty-format (task-instance-runner-script task-instance))))))

                      (li (details ((open ""))
                                   (summary (b "logs"))
                                   (pre ((class "scroll"))
                                        ,(for/fold ((acc ""))
                                                   ((line (in-list logs)))
                                           (string-append acc (format "~a | ~a\n"
                                                                      (datetime->iso8601 (task-instance-log-timestamp line))
                                                                      (task-instance-log-message line)))))))
                      (li (details
                           (summary (b "resources"))
                           (div ((class "task-instance-resources"))
                                ,(xexpr-task-instance-resources resources)))))))
          (message-task-instance-not-found id))
      #:title "task instance"))))

(define-route (/tasks/instances/:task-instance-id/kill request)
  #:method post
  (let ((id (http-route-parameter ':task-instance-id)))
    (if (task-instance-kill! (task-instance-get id))
        (redirect-to (format "/tasks/instances/~a" id))
        (response-task-not-found id))))

;;

(when (file-exists? "testbed.db")
  (delete-file "testbed.db"))
(current-db (sqlite3-connect
             #:database "testbed.db"
             #:mode 'create))

(create-all! (current-db))

;;

(void
 (task-create! (make-task #:name "test"
                          #:description "sample test task"
                          #:runner-type 'eval
                          #:runner-script
                          '((begin (displayln "hello world")
                                   (displayln "this task evals scheme code")))
                          #:created-at (now)))
 (task-create! (make-task #:name "test shell"
                          #:description "sample shell test task"
                          #:runner-type 'shell
                          #:runner-script
                          '("bash" ("-xec" "date; ls -la"))
                          #:created-at (now)))
 (task-create! (make-task #:name "test shell always fail"
                          #:description "sample shell test task which always fails"
                          #:runner-type 'shell
                          #:runner-script
                          '("bash" ("-xec" "sleep 3; exit 1"))
                          #:created-at (now)))
 (task-create! (make-task #:name "shell sleep"
                          #:description "sample shell test task which sleeps"
                          #:runner-type 'shell
                          #:runner-script
                          '("bash" ("-xec" "sleep 999996667"))
                          #:created-at (now))
               #:resources (list (make-resource #:type 'vnc)))
 (task-create! (make-task #:name "test-always-fails"
                          #:description "sample failing test task"
                          #:runner-type 'eval
                          #:runner-script
                          '((begin (error "oops")))
                          #:created-at (now)))
 (task-create! (make-task #:name "test-long-task"
                          #:description "sample test task which takes time to execute"
                          #:runner-type 'eval
                          #:runner-script
                          '((begin (sleep 99999999)))
                          #:created-at (now)))
 (task-create! (make-task #:name "xvfb chromium"
                          #:description "sample chromium task"
                          #:runner-type 'shell
                          #:runner-script
                          `("nix-shell"
                            ("--pure"
                             "-p" "xvfb-run"
                             "-p" "x11vnc"
                             "-p" "chromium"
                             "--run"
                             ,(let* ((resolution '(1280 . 1024))
                                     (chromium-flags (list "--no-sandbox"
                                                           (format "--window-size=~a,~a"
                                                                   (cdr resolution)
                                                                   (car resolution))
                                                           "--disable-infobars")))
                                (string-join `(,(format "xvfb-run --server-args '-screen 0 ~ax~ax24'"
                                                        (car resolution)
                                                        (cdr resolution))
                                               "bash -ec '"
                                               "x11vnc -bg -forever -nopw -quiet -listen localhost -xkb &"
                                               "chromium" ,(string-join chromium-flags " ") "; wait"
                                               "'")
                                             " "))))
                          #:created-at (now))
               #:resources (list (make-resource #:type 'vnc)))

 ;; reset any current host task states to "error" before starting
 (apply update! (current-db)
        (let ((query (~> (from task-instance #:as t)
                         (where (and (= t.host ,(gethostname))
                                     (or (= t.state ,(hash-ref task-states-hash 'pending))
                                         (= t.state ,(hash-ref task-states-hash 'running))))))))
          (for/list ((task-instance (in-entities (current-db) query)))
            (update-task-instance-state task-instance (thunk* 'error))))))

(define stop
  (serve
   #:dispatch (sequencer:make
               (filter:make #rx"^/favicon\\.ico$" (lambda (conn request) (response/empty)))
               (filter:make #rx"^/static/" static-dispatcher)
               (dispatch/servlet http-dispatch-route))
   #:listen-ip "127.0.0.1"
   #:port 8000))
