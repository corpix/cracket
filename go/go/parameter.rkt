#lang racket/base
(require racket/set
         "type.rkt")
(provide (all-defined-out))

(define *transform-depth*  (make-parameter 16))
(define *transformers*     (make-parameter null))
(define *phases*           (make-parameter '(define expand)))
(define *extension*        (make-parameter ".golisp"))
(define *registry*         (make-parameter (make-hasheq)))

;;

(define *env*     (make-parameter (env #f #f (make-hasheq))))
(define *ns*      (make-parameter (env-ns (*env*))))
(define *package* (make-parameter (env-package (*env*))))
(define *macro*   (make-parameter (env-macro (*env*))))

(define *prelude*  (make-parameter (box (list))))
(define *epilogue* (make-parameter (box (list))))
