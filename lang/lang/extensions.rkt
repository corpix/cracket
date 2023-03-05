#lang racket

;; various language extensions which I have collected or want to implement (they could be incomplete or dirty)


;; define which supports supplied-p predicate for opotional positional arguments (kw arguments is to be implemented)
(define-syntax (define* stx)
  (define (argument-aux stx)
    (syntax-case stx ()
      ((sym default-value) (list #'(sym default-value) #f #f #f))
      ((sym default-value supplied?-sym)
       (with-syntax ((supplied?-gensym (format-id #'sym "defaut-~a" #'sym)))
         (list #'(sym supplied?-gensym)
               #'(supplied?-gensym (gensym))
               #'(supplied?-sym (not (eq? supplied?-gensym sym)))
               #'(sym (if supplied?-sym sym default-value)))))
      (argument (list #'argument #f #f #f))))
  (syntax-case stx ()
    ((_ (name argument ...) body ...)
     (let* ((syntaxes (map argument-aux (syntax->list #'(argument ...))))
            (arguments (map car syntaxes))
            (gensyms (filter identity (map cadr syntaxes)))
            (supplied? (filter identity (map caddr syntaxes)))
            (argument-values (filter identity (map cadddr syntaxes)))
            (outer (if (null? gensyms) identity
                       (lambda (inner-stx)
                         (quasisyntax (let ((unsyntax-splicing gensyms))
                                        (unsyntax inner-stx))))))
            (inner (if (null? supplied?) identity
                       (lambda (inner-stx)
                         (quasisyntax (let* ((unsyntax-splicing supplied?)
                                             (unsyntax-splicing argument-values))
                                        (unsyntax-splicing inner-stx)))))))
       (quasisyntax/loc stx
         (define name (unsyntax (outer (quasisyntax
                                        (lambda ((unsyntax-splicing arguments))
                                          (unsyntax (inner #'(body ...)))))))))))))

(define* (hello (name 'corpix name-supplied?))
  (displayln (format "hello, ~a" name))
  (when (not name-supplied?)
    (displayln "I have used default name for greeting")))
