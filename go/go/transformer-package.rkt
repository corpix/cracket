#lang racket/base
(require racket/set
         "type.rkt"
         "parameter.rkt"
         "registry.rkt"
         "package.rkt"
         "env.rkt")
(provide make-package-transformer)

(define current-package #f)

(define (do-define node)
  (begin0 node
    (cond
      ((go:package? node)
       (set! current-package (go:package-name node))
       (unless (registry-has? current-package)
         (let ((env (make-env (ns current-package)
                              (package (make-package current-package))
                              (macro (make-hasheq)))))
           (registry-set! current-package env))))
      ((go:imports? node)
       (let* ((env     (registry-ref current-package))
              (package (env-package env))
              (imports (package-imports package)))
         (for ((import (in-list (go:imports-imports node))))
           (set-package-imports!
            package
            (set-add imports (go:import-package import)))))))))

(define (do-expand node)
  (begin0 node
    (cond
      ((go:package? node)
       ;; package-get + create env + attach to package in registry
       (let ((env (registry-ref (go:package-name node))))
         ;; WORK HERE
         (void))))))

(define (make-package-transformer)
  (transformer 'package
               (map proc
                    `(define expand)
                    `(,do-define ,do-expand))))
