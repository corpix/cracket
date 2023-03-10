#lang racket
(require corpix/syntax
         racket/async-channel
         (for-syntax racket
                     corpix/syntax))
(provide define-task
         current-task
         task-logger
         sequential
         concurrent
         ->
         ->>
         chain
         command
         port
         timeout
         (struct-out task))
(define-logger task)

(define-struct task
  (name meta)
  #:transparent
  #:constructor-name make-task)

(define current-task (make-parameter #f))

(define-syntax (define-task stx)
  (syntax-parse stx
    #:datum-literals (transformer runner)
    ((_ name:id
        (~optional (~seq #:literals literals) #:defaults ((literals #'())))
        (~optional (~seq #:meta meta) #:defaults ((meta #'#f)))
        ((argument ...) body) ...)
     (with-syntax ((*** (quote-syntax ...))
                   (name-aux (format-id #'name "~a-aux" #'name)))
       (syntax/loc stx
         (begin
           (define-syntax (name-aux stx)
             (syntax-parse stx
               #:datum-literals literals
               ((_ argument ...) body) ...))
           (define-syntax (name stx)
             (syntax-parse stx
               #:datum-literals (name)
               ((name (~optional (~seq #:description description) #:defaults ((description #''name)))
                      expr ***)
                (syntax/loc stx
                  (parameterize ((current-task (make-task 'name meta))
                                 (current-logger (make-logger 'name (current-logger))))
                    (dynamic-wind
                      void
                      (thunk (log-debug "beginning execution of ~s" description)
                             (name-aux expr ***))
                      (thunk (log-debug "finished execution of ~s" description))))))))))))))

;;

(define-task sequential
  ((task ...)
   (syntax (begin task ...))))

(define-task concurrent
  ((task ...)
   (syntax
     (let* ((exn-chan (make-async-channel))
            (threads (list
                      (let* ((exn-handler (lambda (exn) (async-channel-put exn-chan exn))))
                        (thread (thunk (with-handlers ((exn? exn-handler)) task)))) ...)))
       (dynamic-wind
         void
         (thunk (for ((job (in-list threads)))
                  (let ((result (sync (choice-evt exn-chan job))))
                    (when (exn? result)
                      (raise result)))))
         (thunk (for ((job (in-list threads)))
                  (kill-thread job))))))))

(define-task ->
  ((task)
   (syntax task))
  ((task (sub-task arguments ...) rest ...)
   (syntax (-> (sub-task task arguments ...) rest ...)))
  ((task sub-task rest ...)
   (syntax (-> (sub-task task) rest ...))))

(define-task ->>
  ((task)
   (syntax task))
  ((task (sub-task arguments ...) rest ...)
   (syntax (->> (sub-task arguments ... task) rest ...)))
  ((task sub-task rest ...)
   (syntax (->> (sub-task task) rest ...))))

(define-task chain
  #:literals (_)
  ((task)
   (syntax task)) ;; tail case
  ((task (sub-task arguments-before ... _ arguments-after ...) rest ...)
   (syntax (chain (sub-task arguments-before ... task arguments-after ...) rest ...)))
  ((task sub-task rest ...)
   (syntax (chain (sub-task task) rest ...))))

(define-task command
  ((command (~optional (~seq #:cwd cwd) #:defaults ((cwd #'#f)))
            (~optional (~seq #:interpreter interpreter) #:defaults ((interpreter #'"bash")))
            (~optional (~seq #:interpreter-arguments interpreter-arguments) #:defaults ((interpreter-arguments #''("-e"))))
            (~optional (~seq #:name name) #:defaults ((name #'#f))))
   (syntax (let* ((custodian (make-custodian (current-custodian)))
                  (command-file (make-temporary-file))
                  (log-prefix (string-append (or name command))))
             (parameterize ((current-custodian custodian)
                            (current-subprocess-custodian-mode 'kill)
                            (subprocess-group-enabled #t))
               (dynamic-wind
                 void
                 (thunk (file-or-directory-permissions command-file #o600)
                        (with-output-to-file command-file
                          (thunk (write-string command))
                          #:exists 'truncate)
                        (let ((arguments (append interpreter-arguments (list (path->string command-file)))))
                          (match-let (((list out in pid err control)
                                       (let ((interpreter-absolute-path (find-executable-path interpreter)))
                                         (unless interpreter-absolute-path
                                           (error (format "can not find absolute path for interpreter ~s in PATH" interpreter)))
                                         (parameterize ((current-directory (or cwd (current-directory))) ;; FIXME: eliminate "or" at transform phase
                                                        (current-custodian custodian))
                                           (apply process* interpreter-absolute-path arguments)))))
                            (close-output-port in)
                            (let* ((io (for/list ((evt (for/list ((port (list out err)))
                                                         (thread (thunk
                                                                  (for ((line (in-lines port)))
                                                                    (log-info "~a: ~a" log-prefix line)))))))
                                         evt)))
                              (control 'wait)
                              (dynamic-wind
                                void
                                (thunk (let ((code (control 'exit-code)))
                                         (when (not (= code 0))
                                           (error (format "shell command '~a' exited with ~a code, err: ~a"
                                                          (append (list interpreter) arguments) code
                                                          (string-trim (port->string err) "\n"
                                                                       #:left? #t
                                                                       #:right? #t))))))
                                (thunk (for ((port (in-list io)))
                                         (sync port))
                                       (close-input-port out)
                                       (close-input-port err)))))))
                 (thunk (custodian-shutdown-all custodian)
                        (delete-file command-file))))))))

(define-task port
  ((identifier task
               (~optional (~seq #:protocol protocol) #:defaults ((protocol #'tcp)))
               (~optional (~seq #:hostname hostname) #:defaults ((hostname #'"localhost")))
               (~optional (~seq #:retry retry) #:defaults ((retry #'30)))
               (~optional (~seq #:wait wait) #:defaults ((wait #'1))))
   (with-syntax ((wait (case (syntax->datum #'protocol)
                         ((tcp)
                          (syntax (let loop ((retries-left retry))
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
                                                           'protocol hostname identifier retry))))))))
                         (else (error (format "unsupported port protocol ~a" (syntax->datum #'protocol)))))))
     (syntax (begin wait task)))))

(define-task timeout
  ((duration:number task)
   (syntax
    (parameterize ((current-custodian (make-custodian (current-custodian))))
      (let* ((exn-chan (make-async-channel))
             (exn-handler (lambda (exn) (async-channel-put exn-chan exn)))
             (task-job (thread (thunk (with-handlers ((exn? exn-handler)) task))))
             (timer-job (thread (thunk (unless (sync/timeout duration task-job)
                                         (with-handlers ((exn? exn-handler))
                                           (error (format "task ~s timed out after ~a seconds"
                                                          (car 'task) duration))))))))
        (dynamic-wind
          void
          (thunk (let ((result (sync (choice-evt exn-chan task-job))))
                   (when (exn? result)
                     (raise result))))
          (thunk (custodian-shutdown-all (current-custodian)))))))))
