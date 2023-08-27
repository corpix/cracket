#lang racket
(require (for-syntax corpix/syntax))
(provide (all-defined-out))

(define current-configuration (make-parameter #f))
(define current-configuration-path (make-parameter (path->complete-path "config.rkt")))
(define current-configurables (make-parameter (make-hasheq)))

(define (config key (default #f))
  (unless (current-configuration)
    (current-configuration
     (if (file-exists? (current-configuration-path))
         (with-input-from-file (current-configuration-path)
           (thunk (read)))
         null)))
  (let ((value (assoc key (current-configuration))))
    (if value (cdr value) default)))

(define-syntax (define-configurable stx)
  (syntax-parse stx
    ((_ key default)
     (syntax
      (begin
        (define key
          (make-derived-parameter
           (make-parameter default) values
           (lambda (value) (or (config 'key) value))))
        (hash-set! (current-configurables) 'key key))))))
