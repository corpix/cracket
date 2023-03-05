#lang racket/base
(require racket/set
         syntax/parse
         "type.rkt"
         "parameter.rkt"
         (for-syntax racket/base
                     racket/set
                     racket/string
                     syntax/parse
                     "type.rkt"
                     "parameter.rkt"))
(provide make-env
         with-env
         with-empty-env)

(begin-for-syntax
  (define (key->parameter key)
    (list (string->symbol (format "*~a*" key)))))

(define-syntax (make-env stx)
  (syntax-parse stx
    ((_ (key:id value:expr) ...)
     (let* ((arguments (make-hasheq
                        (for/list ((k (in-list (attribute key)))
                                   (v (in-list (attribute value))))
                          (cons (syntax->datum k) v))))
            (parameter (lambda (k)
                         (or
                          (hash-ref arguments k #f)
                          (key->parameter k))))
            (bindings (map parameter env-keys)))
       (let ((unexpected (set-subtract (hash-keys arguments) env-keys)))
         (unless (null? unexpected)
           (raise-syntax-error
            #f (format "unexpected arguments ~a" unexpected) stx)))
       (quasisyntax (env (unsyntax-splicing bindings)))))
    ((_ (~seq key:keyword value:expr) ...)
     (quasisyntax (make-env
                   (unsyntax-splicing
                    (for/list ((k (in-list (attribute key)))
                               (v (in-list (attribute value))))
                      (list (string->symbol (keyword->string (syntax->datum k))) v))))))))

(define-syntax (with-env stx)
  (syntax-parse stx
    ((_ env:id ((key:id value:expr) ...) body ...)
     (let ((bindings (make-hasheq
                      (map cons
                           (attribute key)
                           (attribute value)))))
       (quasisyntax
        (with-env
          ((unsyntax-splicing (map (lambda (k)
                                     (let* ((key     (syntax->datum k))
                                            (default (list (string->symbol (format "env-~a" key)) '(*env*)))
                                            (value   (hash-ref bindings key default)))
                                       (list key value)))
                                   (attribute key))))
          body ...))))
    ((_ ((key:id value:expr) ...) body ...)
     (let* ((binding (lambda (k v) (append (key->parameter (syntax->datum k)) (list v))))
            (arguments  (map list
                             (attribute key)
                             (attribute value)))
            (parameters (map binding
                             (attribute key)
                             (attribute value))))
       (quasisyntax
        (parameterize ((unsyntax-splicing parameters)
                       (*env* (make-env (unsyntax-splicing arguments))))
          body ...))))))

(define-syntax (with-empty-env stx)
  (syntax-parse stx
    ((_ body ...)
     (syntax (with-env ((ns      #f)
                        (package #f)
                        (macro   (make-hasheq)))
               body ...)))))

(module+ test
  (require racket/function
           rackunit
           "tool.rkt")
  (run-suites
   (list
    (test-suite "make-env"
                (check-equal? (parameterize ((*ns* 'foo)) (env-ns (make-env)))
                              'foo)
                (check-equal? (parameterize ((*ns* 'foo)) (env-ns (make-env (ns 'bar))))
                              'bar)
                (check-equal? (parameterize ((*ns* 'foo)) (env-ns (make-env #:ns 'bar)))
                              'bar)
                (check-equal? (exn:fail:syntax?
                               (with-handlers ((exn? identity))
                                 (eval '(make-env (nonexistent 'argument)))))
                              #t))
    (test-suite "with-env"
                (parameterize ((*env* (make-env)))
                  (check-equal? (with-env ((ns 'foo)) (*ns*)) 'foo)
                  (check-equal? (env-ns (*env*)) #f)
                  (let ((name 'bar))
                    (check-equal? (with-env ((ns name)) (*ns*))
                                  name))))

    (test-suite "with-empty-env"
                (with-empty-env
                  (check-equal?
                   (*env*)
                   (env #f #f
                        (make-hasheq) (seteq) (make-hasheq)
                        (box (list))  (box (list)))))))))
