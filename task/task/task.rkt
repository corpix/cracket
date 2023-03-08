#lang racket
(require racket/trace
         racket/serialize
         racket/generic
         corpix/syntax
         (for-syntax racket corpix/syntax))
(provide define-task
         define-task-runner
         task-expand
         task-run
         tasks
         (struct-out task))
(module+ test
  (require rackunit))


(define current-tasks-parameters (make-parameter (make-immutable-hasheq)))
(define current-task (make-parameter #f))
(define-for-syntax current-task-transformers (make-parameter (make-hasheq)))
(define current-task-runners (make-parameter (make-hasheq)))
(define current-isolation-methods (make-parameter (make-hasheq)))
(define current-logger (make-parameter (lambda (msg . rest) (displayln (apply format msg rest)))))
(define current-shell-executable (make-parameter "bash"))

;;

(define-syntax-rule (async body ...)
  (let* ((parent (current-thread))
         (exn-value #f)
         (exn-handler (lambda (exn) (set! exn-value exn))))
    (wrap-evt (thread (thunk (with-handlers ((exn? exn-handler)) body ...)))
              (thunk* (when exn-value (raise exn-value))))))

(define (command executable . arguments)
  (match-let (((list out in pid err control)
               (let ((executable-absolute-path (find-executable-path executable)))
                 (unless executable-absolute-path
                   (error (format "can not find absolute path for executable ~s in PATH" executable)))
                 (apply process* executable-absolute-path arguments))))
    (close-output-port in)
    (let* ((logger (current-logger))
           (io (for/list ((evt (for/list ((port (list out err)))
                                 (thread (thunk
                                          (for ((line (in-lines port)))
                                            (logger line)))))))
                 evt)))
      (control 'wait)
      (dynamic-wind void
                    (thunk (let ((code (control 'exit-code)))
                             (when (not (= code 0))
                               (error (format "shell command '~a' exited with ~a code, err: ~a"
                                              (append (list executable) arguments) code
                                              (string-trim (port->string err) "\n"
                                                           #:left? #t
                                                           #:right? #t))))))
                    (thunk (for ((port (in-list io)))
                             (sync port))
                           (close-input-port out)
                           (close-input-port err))))))

(define (alist->shell-arguments arguments)
  (flatten (map
            (lambda (argument)
              (cons (format "--~a" (car argument))
                    (map (lambda (argument)
                           (let loop ((argument argument))
                             (cond
                               ((string? argument) argument)
                               ((symbol? argument) (symbol->string argument))
                               ((number? argument) (number->string argument))
                               ((list? argument) (let ((buf (open-output-string)))
                                                   (write argument buf)
                                                   (get-output-string buf))))))
                         (cdr argument))))
            arguments)))

;;

(define-struct task
  (name body)
  #:prefab
  #:constructor-name make-task)

(define-syntax (define-task-transformer stx)
  (syntax-parse stx
    ((_ (name stx-sym) body ...)
     (syntax/loc stx
       (begin-for-syntax
         (hash-set! (current-task-transformers) 'name
                    (lambda (stx-sym) (syntax-parse stx-sym body ...))))))))

(define-syntax (define-task-runner stx)
  (syntax-parse stx
    ((_ (name argument ... . rest) body ...)
     (syntax/loc stx
       (hash-set! (current-task-runners) 'name
                  (lambda (argument ... . rest) body ...))))))

(define-syntax (define-task stx)
  (syntax-parse stx
    #:datum-literals (transformer runner)
    ((_ name
        (transformer (~optional (~seq #:literals transformer-literals) #:defaults ((transformer-literals #'())))
                     ((transformer-argument ... . transformer-rest) transformer-template) ...)
        (runner (runner-argument ... . runner-rest) runner-body ...))
     (syntax/loc stx
       (begin
         (define-task-transformer (name stx-sym)
           #:datum-literals transformer-literals
           ((_ transformer-argument ... . transformer-rest)
            (syntax/loc stx-sym transformer-template)) ...)
         (define-task-runner (name runner-argument ... . runner-rest)
           runner-body ...))))))


(define-syntax (task-expression-expand stx)
  (syntax-parse stx
    #:datum-literals (format)
    ((_ (format rest ...))
     (syntax/loc stx (format rest ...)))
    ((_ expr) #'expr)))

(define-syntax (task-expand stx)
  (syntax-parse stx
    ((_ #f) #'#f)
    ((_ (task argument ...))
     (let* ((name (syntax->datum #'task))
            (transform (or (hash-ref (current-task-transformers) name #f)
                           (error (format "no task transformer defined for task name ~s" name)))))
       (transform #'(task argument ...))))))

(define (task-run task)
  (and task
       (parameterize ((current-task task))
         (apply (hash-ref (current-task-runners) (task-name task))
                (list task)))))

;;

(define-syntax (define-isolation-method stx)
  (syntax-parse stx
    ((_ (name task . arguments) body ...)
     (with-syntax ((lambda-name (format-id #'name "make-~a-isolation-command" #'name)))
       (syntax/loc stx
         (begin
           (define (lambda-name task . arguments) body ...)
           (hash-set! (current-isolation-methods) 'name lambda-name)))))))

(define-isolation-method (raw task (arguments null))
  (let ((current-file (path->string (srcloc-source (syntax-srcloc #'here)))))
    (append (list (find-executable-path "racket"))
            (alist->shell-arguments (append
                                     `((lib "racket")
                                       (require ,current-file))
                                     arguments
                                     `((eval ,(let ((buf (open-output-string)))
                                                (write `(task-run ',task) buf)
                                                (get-output-string buf)))))))))

(define-isolation-method (bwrap task (arguments null))
  (let* ((current-file (srcloc-source (syntax-srcloc #'here)))
         (current-lib (path->string (path-only (normalize-path current-file))))
         (current-tmpdir "/tmp"))
    (append (list "bwrap")
            (alist->shell-arguments (append
                                     `((clearenv)
                                       (ro-bind "/" "/")
                                       (dev-bind /dev /dev)
                                       (tmpfs ,current-tmpdir)
                                       (tmpfs ,(getenv "HOME"))
                                       (setenv PATH ,(getenv "PATH"))
                                       (setenv HOME ,(getenv "HOME"))
                                       (setenv TMP ,current-tmpdir)
                                       (setenv TMPDIR ,current-tmpdir)
                                       (die-with-parent)
                                       (ro-bind ,current-lib ,current-lib))
                                     arguments))
            (make-raw-isolation-command task))))

;;

(define-struct shell-task
  (command cwd interpreter interpreter-arguments filename)
  #:prefab
  #:constructor-name make-shell-task)

(define-task shell
  (transformer ((command (~optional (~seq #:cwd cwd) #:defaults ((cwd #'#f)))
                         (~optional (~seq #:interpreter interpreter) #:defaults ((interpreter #'"bash")))
                         (~optional (~seq #:interpreter-arguments interpreter-arguments) #:defaults ((interpreter-arguments #''("-e"))))
                         (~optional (~seq #:filename filename) #:defaults ((filename #'#f))))
                (make-task 'shell (make-shell-task command cwd interpreter interpreter-arguments filename))))
  (runner (task)
          (let* ((body (task-body task))
                 (command (shell-task-command body))
                 (command-file (make-temporary-file))
                 (cwd (shell-task-cwd body))
                 (interpreter (shell-task-interpreter body))
                 (interpreter-arguments (shell-task-interpreter-arguments body))
                 (filename (shell-task-filename body))
                 (sem (make-semaphore))
                 (custodian (make-custodian (current-custodian))))
            (file-or-directory-permissions command-file #o600)
            (dynamic-wind
              void
              (thunk
               (with-output-to-file command-file
                 (thunk (write-string command))
                 #:exists 'truncate)
               (let* ((logger (current-logger))
                      (logger-prefix (or filename command))
                      (interpreter-path (find-executable-path interpreter)))
                 (unless interpreter-path
                   (error (format "can not find path for iterpreter ~s in PATH" interpreter)))
                 (let ((command-line (append (cons (path->string interpreter-path) interpreter-arguments)
                                             (list (path->string command-file)))))
                   (parameterize ((current-logger (lambda (str . rest)
                                                    (apply logger
                                                           (string-append logger-prefix ": " str)
                                                           rest))))
                     (match-let (((list out in pid err control)
                                  (parameterize ((current-directory (or cwd (current-directory)))
                                                 (current-custodian custodian))
                                    (apply process* command-line))))
                       (close-output-port in)
                       (let* ((logger (current-logger))
                              (io (for/list ((evt (for/list ((port (list out err)))
                                                    (thread (thunk
                                                             (for ((line (in-lines port)))
                                                               (logger line)))))))
                                    evt)))
                         (control 'wait)
                         (let ((code (control 'exit-code)))
                           (when (not (= code 0))
                             (error (format "shell command '~a' exited with ~a code, err: ~s"
                                            (string-join command-line " ") code
                                            (string-trim (port->string err) "\n"
                                                         #:left? #t
                                                         #:right? #t)))))
                         (for ((port (in-list io)))
                           (sync port))))))))
              (thunk
               (semaphore-post sem)
               (custodian-shutdown-all custodian)
               (delete-file command-file))))))

(define-task git
  (transformer ((action . body)
                (make-task 'git (cons action 'body))))
  (runner (task)
          (apply command "git" (task-body task))))

(define-struct port-task
  (protocol identifier hostname retry wait inner)
  #:prefab
  #:constructor-name make-port-task)

(define-task port
  (transformer ((protocol identifier task
                          (~optional (~seq #:hostname hostname) #:defaults ((hostname #'"localhost")))
                          (~optional (~seq #:retry retry) #:defaults ((retry #'30)))
                          (~optional (~seq #:wait wait) #:defaults ((wait #'1))))
                (make-task 'port (make-port-task protocol identifier hostname retry wait (task-expand task)))))
  (runner (task)
          (let* ((body (task-body task))
                 (protocol (port-task-protocol body))
                 (identifier (port-task-identifier body))
                 (hostname (port-task-hostname body))
                 (retry (port-task-retry body))
                 (wait (port-task-wait body))
                 (inner (port-task-inner body)))
            (case protocol
              ((tcp)
               (let loop ((retries-left retry))
                 (let* ((failed? #f)
                        (custodian (make-custodian))
                        (job (parameterize ((current-custodian custodian))
                               (thread (thunk
                                        (with-handlers ((exn? (thunk* (set! failed? #t))))
                                          (tcp-connect hostname identifier)))))))
                   (unless (and (sync/timeout wait job) (not failed?))
                     (custodian-shutdown-all custodian)
                     (if (> retries-left 0)
                         (begin
                           (sleep wait)
                           (loop (- retries-left 1)))
                         (error (format "given up waiting for ~a port (~a:~a) to become available after ~a retries"
                                        protocol hostname identifier retry)))))))
              (else (error (format "unsupported port protocol ~a" protocol))))
            (task-run inner))))

(define-struct timeout-task
  (duration inner)
  #:prefab
  #:constructor-name make-timeout-task)

(define-task timeout
  (transformer ((duration task)
                (make-task 'timeout (make-timeout-task duration (task-expand task)))))
  (runner (task)
          (let* ((body (task-body task))
                 (duration (timeout-task-duration body))
                 (inner (timeout-task-inner body))
                 (name (task-name inner))
                 (custodian (make-custodian))
                 (sem (make-semaphore))
                 (job (async (unless (sync/timeout duration sem)
                               (custodian-shutdown-all custodian)
                               (error (format "task ~s timed out after ~a seconds" name duration))))))
            (dynamic-wind
              void
              (thunk (parameterize ((current-custodian custodian)
                                    (current-subprocess-custodian-mode 'kill)
                                    (subprocess-group-enabled #t))
                       (thread (thunk (dynamic-wind
                                        void
                                        (thunk (task-run inner))
                                        (thunk (semaphore-post sem)))))))
              (thunk (sync job))))))

(define-struct wait-task
  (duration inner)
  #:prefab
  #:constructor-name make-wait-task)

(define-task wait
  (transformer ((duration task)
                (make-task 'wait (make-wait-task duration (task-expand task)))))
  (runner (task)
          (let* ((body (task-body task))
                 (duration (wait-task-duration body))
                 (inner (wait-task-inner body)))
            (sleep duration)
            (task-run inner))))

(define-struct let-task
  (bindings inner)
  #:prefab
  #:constructor-name make-let-task)

(define-task let
  (transformer ((((sym val) ...) task ...)
                (make-task 'let (make-let-task '((sym val) ...) '(tasks task ...)))))
  (runner (task)
          (let* ((body (task-body task))
                 (bindings (let-task-bindings body))
                 (inner (let-task-inner body))
                 (namespace (make-base-namespace))
                 (module (srcloc-source (syntax-srcloc #'here))))
            (parameterize ((current-namespace namespace))
              (namespace-require `(file ,(path->string module)))
              (for ((binding (in-list bindings)))
                (namespace-set-variable-value! (car binding) (cadr binding)))
              (eval `(tasks ,inner))))))

(define-struct try-task
  (inner except finally)
  #:prefab
  #:constructor-name make-try-task)

(define-task try
  (transformer ((task (~optional (~seq #:except except) #:defaults ((except #'#f)))
                      (~optional (~seq #:finally finally) #:defaults ((finally #'#f))))
                (make-task 'try (make-try-task (task-expand task)
                                               (task-expand except)
                                               (task-expand finally)))))
  (runner (task)
          (let* ((body (task-body task))
                 (inner (try-task-inner body))
                 (except (try-task-except body))
                 (finally (try-task-finally body))
                 (custodian (make-custodian)))
            (parameterize ((current-custodian custodian))
              (dynamic-wind
                void
                (thunk (with-handlers ((exn? (thunk* (task-run except))))
                         (task-run inner)))
                (thunk (task-run finally)
                       (custodian-shutdown-all custodian)))))))

(define-task isolate
  (transformer ((isolation task arguments ...)
                (make-task 'isolate (list isolation (task-expand task) '(arguments ...)))))
  (runner (task)
          (let* ((arguments (task-body task))
                 (name (car arguments))
                 (inner-task (cadr arguments))
                 (isolation-arguments (caddr arguments))
                 (isolation-command-maker
                  (or (hash-ref (current-isolation-methods) name #f)
                      (error (format "isolation method is not defined: ~s" name)))))
            (parameterize ((current-subprocess-custodian-mode 'kill)
                           (subprocess-group-enabled #t))
              (apply command (isolation-command-maker inner-task isolation-arguments))))))

(define-task sequential
  (transformer ((task ...)
                (make-task 'sequential (list (task-expand task) ...))))
  (runner (task)
          (for ((sub-task (task-body task)))
            (task-run sub-task))))

(define-task concurrent
  (transformer ((task ...)
                (make-task 'concurrent (list (task-expand task) ...))))
  (runner (task)
          (parameterize ((current-custodian (make-custodian)))
            (dynamic-wind
              void
              (thunk (for ((job (in-list (for/list ((sub-task (task-body task)))
                                           (async (task-run sub-task))))))
                       (sync job)))
              (thunk (custodian-shutdown-all (current-custodian)))))))

(define-task ->
  (transformer ((task)
                (make-task '-> (task-expand task)))
               ((task (wrapper arguments ...) rest ...)
                (task-expand (-> (wrapper task arguments ...) rest ...)))
               ((task wrapper rest ...)
                (task-expand (-> (wrapper task) rest ...))))
  (runner (task)
          (task-run (task-body task))))

(define-task ->>
  (transformer ((task)
                (make-task '->> (task-expand task)))
               ((task (wrapper arguments ...) rest ...)
                (task-expand (->> (wrapper arguments ... task) rest ...)))
               ((task wrapper rest ...)
                (task-expand (->> (wrapper task) rest ...))))
  (runner (task)
          (task-run (task-body task))))

(define-task chain
  (transformer #:literals (_)
               ((task)
                (make-task 'chain (task-expand task)))
               ((task (wrapper arguments-before ... _ arguments-after ...) rest ...)
                (task-expand (chain (wrapper arguments-before ... task arguments-after ...) rest ...)))
               ((task wrapper rest ...)
                (task-expand (chain (wrapper task) rest ...))))
  (runner (task)
          (task-run (task-body task))))

(define-task eval
  ;; NOTE: eval is limited in terms of the namespaces you could use
  ;; because we should stay serializable (each task COULD be isolated)
  ;; this is why we can not pass non primitive values here
  ;; but this may be changed via some sort of RPC
  ;; this will allow eg use shared procedures which can not be serialized
  (transformer ((expr ...)
                (make-task 'eval '(begin expr ...))))
  (runner (task)
          (eval (task-body task)
                (make-base-namespace))))

(define-syntax-rule (tasks exprs ...)
  (task-run (task-expand (sequential exprs ...))))
