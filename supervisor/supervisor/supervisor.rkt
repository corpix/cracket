#lang racket/base
(require racket/function
         racket/match
         racket/async-channel
         syntax/parse
         library/runtime/exn
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))
(provide define/supervisor
         with-heartbeat
         supervisor
         supervise
         current-supervisor-name)

(define current-supervisor-name (make-parameter (void)))

(begin-for-syntax
  (define-splicing-syntax-class kw-name
    #:attributes (name)
    (pattern (~seq #:name name:expr)))
  (define-splicing-syntax-class kw-arguments
    #:attributes (arguments)
    (pattern (~seq #:arguments (argument:id ...)) #:attr arguments (syntax (argument ...)))
    (pattern (~seq) #:attr arguments (syntax ())))
  (define-splicing-syntax-class kw-scheduler
    #:attributes (fails interval)
    (pattern (~seq (~optional (~seq #:fails    fails:number)    #:defaults ((fails    (syntax 0))))
                   (~optional (~seq #:interval interval:number) #:defaults ((interval (syntax 0)))))))
  (define-splicing-syntax-class kw-heartbeat
    #:attributes (interval fouls)
    (pattern (~seq (~optional (~seq #:interval interval:number) #:defaults ((interval 5)))
                   (~optional (~seq #:fouls    fouls:number)    #:defaults ((fouls    0)))))))

(define-syntax (define/supervisor stx)
  (syntax-parse stx
    ((define/supervisor (name:id arguments:id ...)
       scheduler:kw-scheduler
       ((body0 body ...) ...))
     (quasisyntax (define name
                    (supervisor
                     #:name 'name
                     #:arguments (arguments ...) (unsyntax-splicing (syntax scheduler))
                     ((body0 body ...) ...)))))))

(define-syntax (supervisor stx)
  (syntax-parse stx
    ((supervisor name:kw-name
                 arguments:kw-arguments
                 scheduler:kw-scheduler
                 body ...)
     (with-syntax ((name       (attribute name.name))
                   (arguments  (attribute arguments.arguments))
                   (fails      (attribute scheduler.fails))
                   (interval   (attribute scheduler.interval)))
       (syntax (lambda arguments
                 (thread
                  (thunk (supervise name fails interval
                                    (thunk body ...))))))))))

(define-syntax (with-heartbeat stx)
  (syntax-parse stx
    ((with-heartbeat (heartbeat:id settings:kw-heartbeat) inner ...)
     (with-syntax ((syntax/interval (attribute settings.interval))
                   (syntax/fouls    (attribute settings.fouls)))
       (syntax (let* ((interval   syntax/interval)
                      (fouls      syntax/fouls)
                      (chan      (make-async-channel 1))
                      (heartbeat (lambda () (async-channel-put chan (current-milliseconds)))))
                 (sync (thread
                        (thunk (let loop ((current-fouls 0))
                                 (if (sync/timeout interval chan)
                                     (loop 0)
                                     (if (< current-fouls fouls)
                                         (begin
                                           (log-warning "supervisor: '~a' heartbeat: have not received heartbeat for ~a time in ~as time frame"
                                                        (current-supervisor-name) current-fouls interval)
                                           (loop (add1 current-fouls)))
                                         (log-warning "supervisor: '~a' heartbeat: max fouls number ~a reached" (current-supervisor-name) fouls))))))
                       (thread (thunk inner ...)))))))))

;;

(define (supervise name fails interval thunk)
  (let loop ((counter  0)
             (last    (current-milliseconds)))
    (when (> counter fails)
      (error 'supervisor
             "'~a' hit limit of ~a fails in a given ~as time frame"
             name fails interval))
    (when (> counter 0)
      (log-warning "supervisor: '~a' is restarting for ~a time in last ~as" name counter interval))
    (parameterize ((current-custodian      (make-custodian))
                   (current-supervisor-name name))
      (try (sync (thread thunk))
           (finally (custodian-shutdown-all (current-custodian)))))
    (loop
     (if (> (- (current-milliseconds) last) (* 1000 interval)) 1 (add1 counter))
     (current-milliseconds))))
