#lang racket
(require (for-syntax racket
                     "syntax.rkt")
         racket/trace
         racket/serialize
         racket/generic
         "syntax.rkt")
(provide current-log-prefix
         with-log-prefix
         shell-command

         define-task
         task-run
         task-expand
         task-log)

;; agenda:
;; - object serialization: get rid of classes, invent prefab struct with generic dispatcher (runnable)
;; - isolation (may we have a raco distrobute?)
;; - output logging (current-output-port, but may conflict with isolation?)
;; - build graph (inputs & outputs)
;;   - wrap each input result into a promise so it will be memoized
;; - loading build plan
;;   - path traversal attacks mitigation
;;   - running inside containers
;;     - backend in separate language (translate config to rpc calls or different language)
;;     - raco distribute

(define current-log-prefix (make-parameter #f))
(define-syntax with-log-prefix
  (syntax-rules ()
    ((_ prefix body ...)
     (let ((parent-prefix (current-log-prefix)))
       (parameterize ((current-log-prefix (if parent-prefix
                                              (format "~a~a: " parent-prefix prefix)
                                              (format "~a: " prefix))))
         body ...)))))

(define (shell-command executable . arguments)
  (let-values (((p out in err)
                (apply subprocess #f #f #f (find-executable-path executable)
                       arguments)))
    (close-output-port in)
    (subprocess-wait p)
    (let ((code (subprocess-status p)))
      (when (not (= code 0))
        (error (format "shell command '~a' exited with ~a code, err: ~a"
                       (append (list executable) arguments) code
                       (string-trim (port->string err) "\n"
                                    #:left? #t
                                    #:right? #t)))))
    (for/list ((evt (for/list ((port (list out err)))
                      (thread (lambda ()
                                (for ((line (in-lines port)))
                                  (task-log line)))))))
      (sync evt))))

(define (task-log message)
  (let ((prefix (current-log-prefix)))
    (when prefix (display prefix))
    (displayln message)))

;;

(define-struct task
  (name thunk inputs outputs)
  #:transparent)

(define-syntax (define-task stx)
  (syntax-parse stx
    ((_ (name argument ...) body ...)
     (with-syntax ((maker (format-id #'name "make-~a-task" #'name)))
       (syntax/loc stx
         (define (maker argument ...
                        #:inputs (inputs null)
                        #:outputs (outputs null))
           (let* ((runner (thunk body ...)))
             (make-task 'name runner inputs outputs))))))))

(define-syntax (task-expand stx)
  (let* ((task #f)
         (inputs #f)
         (outputs #f)
         (construct (lambda (ctor arguments)
                      (with-syntax ((inputs inputs)
                                    (outputs outputs)
                                    (ctor ctor)
                                    ((argument ...) arguments))
                        (syntax/loc stx
                          (ctor argument ...
                                #:inputs inputs
                                #:outputs outputs))))))
    (let loop ((stx stx))
      (syntax-parse stx
        #:datum-literals (task-expand
                          let include
                          shell git-clone
                          sequential concurrent)
        ((task-expand (expr ...))
         (loop (syntax (expr ...))))
        ((task-expand expr:id)
         (syntax expr))
        ((let ((sym expr) ...) body ...)
         (syntax
          (let ((sym (task-expand expr)) ...)
            (task-expand body) ...)))
        ((include path)
         (let* ((load-path (string->path (syntax->datum #'path)))
                (load-path-base (syntax-srcdir #'path))
                (load-path-abs (normalize-path (build-path load-path-base load-path)))
                (source (find-relative-path load-path-base load-path-abs)))
           (with-syntax ((expr (replace-context #'self
                                                (call-with-input-string
                                                 (file->string load-path-abs)
                                                 (lambda (port)
                                                   (read-syntax load-path-abs port))))))
             #'(task-expand expr))))
        ((before ...+ #:inputs (input ...) after ...)
         (set! inputs #'(list (task-expand input) ...))
         (loop #'(before ... after ...)))
        ((before ...+ #:outputs (output ...) after ...)
         (set! outputs #'(list (task-expand output) ...))
         (loop #'(before ... after ...)))
        ((shell argument ...)
         (let loop ((argstx #'(argument ...)))
           (syntax-parse argstx
             #:context stx
             #:datum-literals (file)
             (((file path) argument ...)
              (with-syntax ((expr (file->string (syntax->datum #'path))))
                (loop #'(expr (file path) argument ...))))
             ((argument ...)
              (construct #'make-shell-task #'(argument ...))))))
        ((git-clone argument ...)
         (construct #'make-git-clone-task #'(argument ...)))
        ((sequential argument ...)
         (construct #'make-sequential-task
                    #'((list (task-expand argument) ...))))
        ((concurrent argument ...)
         (construct #'make-concurrent-task
                    #'((list (task-expand argument) ...))))))))

(define (task-run task)
  (let ((inputs (task-inputs task)))
    (when inputs
      (for ((input inputs))
        (task-run input)))
    (begin0 (void)
      ((task-thunk task)))))

;;

(define-task (shell body
                    (file #f)
                    (interpreter "bash")
                    (interpreter-arguments null))
  (let ((body-file (make-temporary-file)))
    (dynamic-wind
      (lambda () (file-or-directory-permissions body-file #o600))
      (lambda ()
        (with-output-to-file
          body-file
          (lambda () (write-string body))
          #:exists 'truncate)
        (with-log-prefix (or file body)
          (apply shell-command
                 (append (cons interpreter interpreter-arguments)
                         (list body-file)))))
      (lambda () (delete-file body-file)))))

(define-task (git-clone url (branch #f))
  (let ((arguments (list url)))
    (apply shell-command
           (append (list "git" "clone" "--progress")
                   (if branch (list "-b" branch) null)
                   (list url)))))
;;

(define-task (sequential tasks)
  (for ((task tasks))
    (sync (thread (thunk (task-run task))))))

(define-task (concurrent tasks)
  (let ((running (for/list ((task tasks))
                   (thread (thunk (task-run task))))))
    (for ((task-thread running)) (sync task-thread))))


;;

(module+ test
  (require rackunit)
  (test-case "task-run"
    (let ((ex (task-expand (let ((dep (shell "echo i am a dependency")))
                             (sequential
                              #:inputs ((shell "echo i am a dependency for sequential") dep)
                              (shell "echo hello")
                              (shell "echo hello2" #:inputs (dep)))))))
      (task-run ex))))
