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
(define task-states '(pending running stopped errored killed))
(define task-states-running '(pending running))

;;

(define-struct runner-job (instance thread) #:transparent)
(define-struct runner-jobs (semaphore store) #:transparent)

(define current-runner-jobs (make-parameter
                             (make-runner-jobs (make-semaphore 1)
                                               (make-hash))))

(define (runner-job-register! id job #:registry (jobs (current-runner-jobs)))
  (call-with-semaphore
   (runner-jobs-semaphore jobs)
   (thunk (hash-set! (runner-jobs-store jobs) id job))))

(define (runner-jobs-unregister! id #:registry (jobs (current-runner-jobs)))
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
    ((_ id:id (fields ...)
        (~seq #:executor executor-expr))
     (with-syntax ((struct-name (format-id #'id "runner-~a" #'id))
                   (constructor-sym (format-id #'id "make-~a" #'id))
                   (executor-sym (format-id #'id "~a-execute" #'id)))
       (syntax (begin
                 (define-struct struct-name (fields ...) #:constructor-name constructor-sym)
                 (define executor-sym executor-expr)
                 (hash-set! (current-runners) 'id
                            (make-hash (list (cons 'constructor constructor-sym)
                                             (cons 'executor executor-sym))))))))))

(define-runner eval (code)
  #:executor (lambda (runner) (void (eval (runner-eval-code runner)))))

(define-runner shell (command arguments)
  #:executor (lambda (runner)
               (match-let* ((command (runner-shell-command runner))
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
                          (close-input-port err))))))

(define (runner-execute type script)
  (let* ((runner-description (or (hash-ref (current-runners) type #f)
                                 (error (format "runner ~v is not defined" type))))
         (constructor (hash-ref runner-description 'constructor))
         (executor (hash-ref runner-description 'executor)))
    (executor (apply constructor script))))

;;

(define-schema task
  ((id id/f #:primary-key #:auto-increment)
   (name string/f #:contract non-empty-string? #:unique)
   (description string/f)
   (runner-type symbol/f)
   (runner-script sexpr/f)
   (capabilities sexpr/f)
   (created-at datetime/f)
   (updated-at datetime/f #:nullable)))

(define-schema task-instance
  ((id id/f #:primary-key #:auto-increment)
   (name string/f #:contract non-empty-string?)
   (description string/f)
   (task-id id/f #:foreign-key (task id))
   (state (enum/f task-states))
   (runner-type symbol/f)
   (runner-script sexpr/f)
   (capabilities sexpr/f)
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
                       #:runner-type 'eval
                       #:runner-script
                       '((begin (displayln "hello world")
                                (displayln "this task evals scheme code")))
                       #:capabilities '()
                       #:created-at (now)))
   (insert! (current-db)
            (make-task #:name "test shell"
                       #:description "sample shell test task"
                       #:runner-type 'shell
                       #:runner-script
                       '("bash" ("-xec" "date; ls -la"))
                       #:capabilities '()
                       #:created-at (now)))
   (insert! (current-db)
            (make-task #:name "test shell always fail"
                       #:description "sample shell test task which always fails"
                       #:runner-type 'shell
                       #:runner-script
                       '("bash" ("-xec" "sleep 3; exit 1"))
                       #:capabilities '()
                       #:created-at (now)))
   (insert! (current-db)
            (make-task #:name "shell sleep"
                       #:description "sample shell test task which sleeps"
                       #:runner-type 'shell
                       #:runner-script
                       '("bash" ("-xec" "sleep 999996667"))
                       #:capabilities '()
                       #:created-at (now)))
   (insert! (current-db)
            (make-task #:name "test-always-fails"
                       #:description "sample failing test task"
                       #:runner-type 'eval
                       #:runner-script
                       '((begin (error "oops")))
                       #:capabilities '()
                       #:created-at (now)))
   (insert! (current-db)
            (make-task #:name "test-long-task"
                       #:description "sample test task which takes time to execute"
                       #:runner-type 'eval
                       #:runner-script
                       '((begin (sleep 99999999)))
                       #:capabilities '()
                       #:created-at (now)))
   (insert! (current-db)
            (make-task #:name "xvfb chromium"
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
                       #:capabilities '((vnc . 6666))
                       #:created-at (now))))

 ;; reset any current host task states to "error" before starting
 (apply update! (current-db)
        (let ((query (~> (from task-instance #:as t)
                         (where (and (= t.host ,(gethostname))
                                     ;; fixme: index-of will not work for postgres
                                     ;; need to infer enum type in query constructor
                                     (or (= t.state ,(index-of task-states 'pending))
                                         (= t.state ,(index-of task-states 'running))))))))
          (for/list ((task-instance (in-entities (current-db) query)))
            (update-task-instance-state task-instance (lambda _ 'error))))))

;;


(define (task-instance-from-task task #:state (state 'pending))
  (make-task-instance #:name (task-name task)
                      #:description (task-description task)
                      #:task-id (task-id task)
                      #:state state
                      #:runner-type (task-runner-type task)
                      #:runner-script (task-runner-script task)
                      #:capabilities (task-capabilities task)
                      #:host (gethostname)
                      #:created-at (now)))

(define (task-list)
  (sequence->list
   (in-entities (current-db)
                (~> (from task #:as t)
                    (order-by ((t.updated-at) (t.created-at)))))))

(define (task-get id)
  (lookup (current-db) (~> (from task #:as t)
                           (where (= t.id ,id)))))

(define (task-instance-create! task)
  (insert-one! (current-db) (task-instance-from-task task)))

(define (task-instance-state-set! instance state)
  (update-one! (current-db) (~> instance
                                (update-task-instance-state _ (thunk* state))
                                (update-task-instance-updated-at _ (thunk* (now)))))
  (task-log-append instance (symbol->string state)))

(define (task-instance-run! instance)
  (let* ((runner-type (task-instance-runner-type instance))
         (runner-script (task-instance-runner-script instance))
         (job-thunk (thunk
                     (task-instance-state-set! instance 'running)
                     (let*-values (((custodian) (make-custodian))
                                   ((cancel-evt) (handle-evt (thread-receive-evt) (thunk* (thread-receive))))
                                   ((in out) (make-pipe))
                                   ((io-pump)
                                    (thread (thunk
                                             (for ((line (in-lines in)))
                                               (task-log-append instance line)))))
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
                                             (close-input-port in)
                                             (sync io-pump)))))
                       (cond
                         ((eq? result 'kill)
                          (task-instance-state-set! instance 'killed))
                         (worker-exn
                          (task-log-append instance (exn-message worker-exn))
                          (task-instance-state-set! instance 'errored))
                         ((thread? result)
                          (task-instance-state-set! instance 'stopped)))))))
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

(define (task-instance-get id)
  (lookup (current-db)
          (~> (from task-instance #:as t)
              (where (= t.id ,id)))))

(define (task-instance-list (task-id #f))
  (let ((query (cond (task-id (~> (from task-instance #:as t)
                                  (where (= t.task-id ,task-id))
                                  (order-by ((t.updated-at) (t.created-at)))))
                     (else (~> (from task-instance #:as t)
                               (order-by ((t.updated-at #:desc) (t.created-at #:desc))))))))
    (sequence->list (in-entities (current-db) query))))

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

(define (task-run! task)
  (let* ((instance (task-instance-create! task)))
    (task-instance-run! instance)))

;;

(define (xexpr-navigation)
  `(nav ((class "navigation"))
        (a ((href "/tasks")) "Tasks")
        (a ((href "/instances")) "Instances")))

(define (css-page)
  (css-expr->css (css-expr (body #:margin (40px auto)
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

                           (a.instance
                            #:position relative
                            #:padding-right 10px
                            #:text-decoration none)

                           (a.instance.state-pending::after
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
                           (a.instance.state-stopped::after
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
                           (a.instance.state-running::after
                            #:content ""
                            #:position absolute
                            #:top 50%
                            #:right 0
                            #:transform |translateY(-50%)|
                            #:width 0
                            #:height 0
                            #:border-style solid
                            #:border-width (6px 0 6px 6px)
                            #:border-color |transparent transparent transparent yellow|)
                           (a.instance.state-errored::after
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
                           (a.instance.state-killed::after
                            #:content ""
                            #:position absolute
                            #:top 50%
                            #:right 0
                            #:transform |translateY(-50%)|
                            #:width 0
                            #:height 0
                            #:border-style solid
                            #:border-width (6px 0 6px 6px)
                            #:border-color |transparent transparent transparent pink|))))

(define (xexpr-page body #:title (title ""))
  `(html (head (title ,title)
               (style ,(css-page)))
         (body (div ((class "root"))
                    (div ((class "container"))
                         (div ((class="header"))
                              ,(xexpr-navigation))
                         (div ((class "content"))
                              ,body))))))

(define (xexpr-instances (task-id #f))
  `(div (ul ,@(for/list ((instance (in-list (task-instance-list task-id))))
                `(li (a ((href ,(format "/instances/~a" (task-instance-id instance)))
                         (class ,(format "instance state-~a" (task-instance-state instance))))
                        ,(task-instance-name instance)))))))

;;

(define (message/task-not-found id)
  (format "task ~a was not found"))

(define (response/task-not-found id)
  (response/xexpr (message/task-not-found id)
                  #:code http-status-not-found))

(define (message/instance-not-found id)
  (format "instance ~a was not found" id))

(define (response/instance-not-found id)
  (response/xexpr (message/instance-not-found id)
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
        (response/task-not-found id))))


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
                    (li (details
                         (summary (b "capabilities"))
                         (div (pre ,(pretty-format (task-capabilities task))))))

                    (li (details (summary (b "instances")) ,(xexpr-instances (task-id task))))))
          (message/task-not-found id))
      #:title "task"))))

(define-route (/instances request)
  #:method get
  (response/xexpr
   (xexpr-page (xexpr-instances) #:title "instances")))

(define-route (/instances/:instance-id request)
  #:method get
  (let* ((id (http-route-parameter ':instance-id))
         (instance (task-instance-get id)))
    (response/xexpr
     #:code (if instance http-status-ok http-status-not-found)
     (xexpr-page
      (if instance
          (let ((logs (task-log-list id)))
            `(div (table (tr (td "name") (td (a ((href ,(format "/tasks/~a" (task-instance-task-id instance))))
                                                ,(task-instance-name instance))))
                         (tr (td "state") (td ,(format "~a" (task-instance-state instance))))
                         (tr (td "runner") (td ,(format "~a" (task-instance-runner-type instance))))
                         (tr (td "host") (td ,(task-instance-host instance)))
                         (tr (td "created-at") (td ,(datetime->iso8601 (task-instance-created-at instance))))
                         (tr (td "updated-at")
                             (td ,(let ((updated-at (task-instance-updated-at instance)))
                                    (if (sql-null? updated-at) "" (datetime->iso8601 updated-at))))))

                  (form ((method "post")
                         (action ,(format "/instances/~a/kill" (task-instance-id instance))))
                        (button ((type "submit")
                                 ,@(if (memq (task-instance-state instance) task-states-running)
                                       null
                                       '((disabled ""))))
                                "kill"))

                  (ul (li (details
                           (summary (b "runner"))
                           (div (pre ,(pretty-format (task-instance-runner-script instance))))))

                      (li (details
                           (summary (b "capabilities"))
                           (div (pre ,(pretty-format (task-instance-capabilities instance))))))

                      (li (details
                           (summary (b "logs"))
                           (pre ((class "scroll"))
                                ,(for/fold ((acc ""))
                                           ((line (in-list logs)))
                                   (string-append acc (format "~a | ~a\n"
                                                              (datetime->iso8601 (task-log-timestamp line))
                                                              (task-log-message line))))))))))
          (message/instance-not-found id))
      #:title "instance"))))

(define-route (/instances/:instance-id/kill request)
  #:method post
  (let ((id (http-route-parameter ':instance-id)))
    (if (task-instance-kill! (task-instance-get id))
        (redirect-to (format "/instances/~a" id))
        (response/task-not-found id))))

(define static-dispatcher
  (let ((url->path/static (make-url->path "static")))
    (files:make #:url->path (lambda (u)
                              (url->path/static
                               (struct-copy url u [path (cdr (url-path u))])))
                #:path->mime-type path-mime-type)))

(define stop
  (serve
   #:dispatch (sequencer:make
               (filter:make #rx"^/static/" static-dispatcher)
               (dispatch/servlet http-dispatch-route))
   #:listen-ip "127.0.0.1"
   #:port 8000))
