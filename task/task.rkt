#lang racket
(require (for-syntax racket
                     "syntax.rkt")
         racket/trace
         racket/serialize
         racket/generic
         "syntax.rkt")
(provide define-task
         define-task-runner
         task-expand
         task-run)
(module+ test
  (require rackunit))


(define current-tasks-parameters (make-parameter (make-immutable-hasheq)))
(define current-task (make-parameter #f))
(define-for-syntax current-task-transformers (make-parameter (make-hasheq)))
(define current-task-runners (make-parameter (make-hasheq)))
(define current-isolation-methods (make-parameter (make-hasheq)))
(define current-logger (make-parameter displayln))
(define current-shell-executable (make-parameter "bash"))

;;

(define (command executable . arguments)
  (match-let (((list out in pid err control)
                (apply process* (find-executable-path executable)
                       arguments)))
    (close-output-port in)
    (let* ((logger (current-logger))
           (io (for/list ((evt (for/list ((port (list out err)))
                                 (thread (lambda ()
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
        (transformer ((transformer-argument ... . transformer-rest) transformer-template) ...)
        (runner (runner-argument ... . runner-rest) runner-body ...))
     (syntax/loc stx
       (begin
         (define-task-transformer (name stx-sym)
           ((_ transformer-argument ... . rest)
            (syntax/loc stx-sym transformer-template)) ...)
         (define-task-runner (name runner-argument ... . runner-rest)
           runner-body ...))))))

(define-syntax (parameterize-tasks-defaults stx)
  (syntax-parse stx
    ((_ ((parameter-name:id parameter-value) ...) body ...)
     (syntax/loc stx
       (let* ((default (gensym))
              (parameters (current-tasks-parameters)))
         (parameterize ((current-tasks-parameters
                         (for/fold ((parameters (current-tasks-parameters)))
                                   ((name (in-list (list 'parameter-name ...)))
                                    (value (in-list (list parameter-value ...))))
                           (if (eq? (hash-ref parameters name default) default)
                               (hash-set parameters name value)
                               parameters))))
           (let ((parameter-name (hash-ref (current-tasks-parameters) 'parameter-name)) ...)
             body ...)))))))

(define-syntax (parameterize-tasks stx)
  (syntax-parse stx
    ((_ ((parameter-name:id parameter-value) ...) body ...)
     (syntax/loc stx
       (let* ((parameters (current-tasks-parameters)))
         (parameterize ((current-tasks-parameters
                         (for/fold ((parameters (current-tasks-parameters)))
                                   ((name (in-list (list 'parameter-name ...)))
                                    (value (in-list (list parameter-value ...))))
                           (hash-set parameters name value))))
           (let ((parameter-name (hash-ref (current-tasks-parameters) 'parameter-name)) ...)
             body ...)))))))


(define-syntax (task-expression-expand stx)
  (syntax-parse stx
    #:datum-literals (format parameter)
    ((_ (format rest ...))
     (syntax/loc stx (format rest ...)))
    ((_ (parameter name:id))
     (syntax/loc stx (hash-ref (current-tasks-parameters) 'name)))
    ((_ expr) #'expr)))

(define-syntax (task-expand stx)
  (syntax-parse stx
    ((_ (task argument ...))
     (let* ((name (syntax->datum #'task))
            (transform (or (hash-ref (current-task-transformers) name #f)
                           (error (format "no task transformer defined for task name ~s" name)))))
       (transform #'(task argument ...))))))

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

(define-task shell
  (transformer ((command:str)
                (make-task 'shell (list command))))
  (runner (task)
          (parameterize-tasks-defaults
           ((interpreter "bash")
            (interpreter-arguments '("-e"))
            (filename #f))

           (let ((body (car (task-body task)))
                 (body-file (make-temporary-file)))
             (dynamic-wind
               (lambda () (file-or-directory-permissions body-file #o600))
               (lambda ()
                 (with-output-to-file
                   body-file
                   (lambda () (write-string body))
                   #:exists 'truncate)
                 (let ((previous-logger (current-logger))
                       (logger-prefix (or filename body)))
                   (parameterize ((current-logger (lambda (str)
                                                    (previous-logger
                                                     (string-append logger-prefix ": " str)))))
                     (apply command
                            (append (cons interpreter interpreter-arguments)
                                    (list body-file))))))
               (lambda () (delete-file body-file)))))))

(define-task git
  (transformer ((action . body)
                (make-task 'git (cons action 'body))))
  (runner (task)
          (apply command "git" (task-body task))))

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
            (apply command (isolation-command-maker inner-task isolation-arguments)))))

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
          (for ((job (in-list (for/list ((sub-task (task-body task)))
                                (thread (thunk (task-run sub-task)))))))
            (sync job))))

;;

(define (task-run task . arguments)
  (parameterize ((current-task task)
                 (current-tasks-parameters (for/fold ((parameters (current-tasks-parameters)))
                                                     ((argument (in-list arguments)))
                                             (hash-set parameters
                                                       (car argument)
                                                       (cadr argument)))))
    (apply (hash-ref (current-task-runners) (task-name task))
           (list task))))

(module+ test
  (task-run (task-expand
             (sequential (shell "whoami")
                         (shell "uname -a"))) ;; => '#s(task sequential (#s(task shell ("whoami")) #s(task shell ("uname -a"))))
            ) ;; => whoami: user
              ;; => uname -a: Linux ran 6.1.3 #1-NixOS SMP PREEMPT_DYNAMIC Wed Jan  4 10:29:02 UTC 2023 x86_64 GNU/Linux

  (task-run (task-expand (concurrent (shell "date && sleep 1 && date")
                                     (shell "date && sleep 1 && date")
                                     (shell "date && sleep 1 && date")
                                     (shell "date && sleep 1 && date")
                                     (shell "date && sleep 1 && date")
                                     (shell "for x in $(seq 1 10); do echo $x; sleep 1; done"))))
  ;;(task-run (task-expand (isolate 'raw (shell "whoami"))))
  )
