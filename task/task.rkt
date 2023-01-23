#lang racket
(require (for-syntax racket
                     "syntax.rkt")
         racket/trace
         racket/serialize
         racket/generic
         "syntax.rkt")
(provide define-task
         define-task-runner
         command
         run-factory
         run-shell
         run-git
         run-isolate
         factory
         shell
         git
         isolate
         task-expand
         task-run)
(module+ test
  (require rackunit))

(define current-shell-executable (make-parameter "bash"))
(define current-task-runners (make-parameter (make-hasheq)))
(define current-task (make-parameter #f))
(define current-isolation-methods (make-parameter (make-hasheq)))
(define current-logger (make-parameter displayln))

;;

(define (command executable . arguments)
  (let-values (((p out in err)
                ;; FIXME: ports are block-buffered
                ;; bad news is we can't change this easily
                ;; good news is we could use files where we could control the reader buffer
                ;; also files will allow us to collect some raw logs which is kinda useful
                ;; but needs a way to return this (maybe each runner could set some meta information for task result?)
                (apply subprocess #f #f #f (find-executable-path executable)
                       arguments)))
    (close-output-port in)
    (let* ((logger (current-logger))
           (io (for/list ((evt (for/list ((port (list out err)))
                                 (thread (lambda ()
                                           (for ((line (in-lines port)))
                                             (logger line)))))))
                 evt)))
      (subprocess-wait p)
      (dynamic-wind void
                    (thunk (let ((code (subprocess-status p)))
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
  (name runner body)
  #:prefab)

(define-syntax (define-task stx)
  (syntax-parse stx
    ((_ (name argument ... . rest) body ...)
     (syntax/loc stx
       (define-syntax (name stx)
         (syntax-parse stx
           ((_ argument ... . rest)
            (syntax/loc stx (begin body ...)))))))))

(define-syntax (define-task-runner stx)
  (syntax-parse stx
    ((_ (name arguments ... . rest) body ...)
     (syntax/loc stx
       (begin
         (define (name arguments ... . rest) body ...)
         (hash-set! (current-task-runners) 'name name))))))

;;

(define-task-runner (run-factory task . arguments)
  (apply (task-body task) arguments))

(define-task-runner (run-shell #:interpreter (interpreter "bash")
                               #:interpreter-arguments (interpreter-arguments '("-e"))
                               #:filename (filename #f)
                               task)
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
      (lambda () (delete-file body-file)))))

(define-task-runner (run-git task)
  (apply command "git" (task-body task)))

(define-task-runner (run-isolate task)
  (let* ((arguments (task-body task))
         (name (car arguments))
         (inner-task (cadr arguments))
         (isolation-arguments (caddr arguments))
         (isolation-command-maker
          (or (hash-ref (current-isolation-methods) name #f)
              (error (format "isolation method is not defined: ~s" name)))))
    (apply command (isolation-command-maker inner-task isolation-arguments))))

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
                                       (lib "racket/serialize")
                                       (require ,current-file))
                                     arguments
                                     `((eval ,(let ((buf (open-output-string)))
                                                (write `(task-run (deserialize ',(serialize task))) buf)
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

(define-task (factory (arguments ...) body)
  (make-task 'factory 'run-factory
             (lambda (arguments ...) body)))

(define-task (shell command)
  (make-task 'shell 'run-shell
             (list command)))

(define-task (git action . body)
  (make-task 'git 'run-git
             (cons action body)))

(define-task (isolate isolation task arguments ...)
  (make-task 'isolate 'run-isolate
             (list isolation task '(arguments ...))))

;;

(define-syntax (task-expand stx) null)

(define (task-run task . arguments)
  (parameterize ((current-task task))
    (let loop ((task task)
               (arguments arguments))
      (set! task
        (apply (hash-ref (current-task-runners) (task-runner task))
               task
               arguments))
      (when (task? task) ;; NOTE: reduce task, but omit arguments (they are valid only on first pass)
        (loop task null)))))

(module+ test
  (task-run (factory (cmd)
                     (isolate 'raw (shell cmd)))
            "whoami"))
