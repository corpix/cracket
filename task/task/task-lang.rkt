#lang racket
(require racket
         corpix/syntax
         "task.rkt")
(provide (except-out (all-from-out racket)
                     read-syntax
                     #%module-begin)
         (rename-out (task-read-syntax read-syntax)
                     (#%task-module-begin #%module-begin)))

(define (task-read-syntax source port)
  (let ((stx (read-syntax source port)))
    (with-syntax ((name #'task-mod)
                  (expander #'"task-lang.rkt"))
      (cond
        ((eof-object? stx) (syntax (module name expander)))
        (else (with-syntax ((expr (replace-context #'self stx)))
                (syntax (module name expander expr))))))))

(define-syntax #%task-module-begin
  (syntax-rules ()
    ((_ expr) (#%module-begin (task-run (task-expand expr))))))
