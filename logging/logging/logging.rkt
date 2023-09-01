#lang racket
(require racket/match
         racket/logging
         corpix/os
         corpix/time)

(provide make-logging
         make-logging-printer
         current-logging-colors?
         current-logging-pid?
         current-logging-timestamp?

         (all-from-out racket/logging))

(define current-logging-colors? (make-parameter #t))
(define current-logging-pid? (make-parameter #t))
(define current-logging-timestamp? (make-parameter #t))

(define (logging-format-level level str)
  (let ((colors? (current-logging-colors?)))
    (string-append
     (if colors?
         (match level ;; FIXME: move to corpix-ansi + detect terminals
           ('trace    "\e[0;35m")
           ('debug    "\e[0;36m")
           ('info     "\e[0;32m")
           ('warning  "\e[0;33m")
           ('error    "\e[0;31m")
           ('fatal    "\e[43m\e[1;91m")
           (else ""))
         "")
     (match str
       ((? string?) str)
       ((? symbol?) (symbol->string str)))
     (if colors? "\e[0m" ""))))

(define-syntax (make-logging-printer stx)
  (syntax-case stx ()
    ((_ logger level topic0 topic ...)
     #'(let* ((receiver (make-log-receiver logger level topic0 topic ...))
              (pid? (current-logging-pid?))
              (pid (and pid? (getpid)))
              (timestamp? (current-logging-timestamp?)))
         (thread (thunk
                  (let loop ()
                    (match (sync receiver (thread-receive-evt))
                      ((vector message-level message-text message-payload message-topic)
                       (display (logging-format-level message-level
                                                      (string-append (symbol->string message-level))))
                       (write-char #\:)
                       (write-char #\space)
                       (when pid?
                         (write-char #\#)
                         (display pid)
                         (write-char #\space))
                       (when timestamp?
                         (display (datetime->string (current-date)))
                         (write-char #\space))
                       (display message-text)
                       (write-char #\newline)
                       (loop))
                      ((app (lambda (evt) (eq? evt (thread-receive-evt))))
                       (void))))))))))

(define-syntax (make-logging stx)
  (syntax-case stx ()
    ((_ level topic0 topic ...)
     #'(let ((logger (make-logger topic0 (current-logger))))
         (void (make-logging-printer logger level topic0 topic ...))
         logger))))
