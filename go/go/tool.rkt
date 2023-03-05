#lang racket/base
(require racket/match
         racket/function
         racket/bool
         racket/string
         racket/list
         racket/format
         racket/port
         rackunit
         rackunit/text-ui
         (for-syntax racket/base))
(provide (all-defined-out)
         (for-syntax (all-defined-out)))

;; test

(define (run-suites suites)
  (for ((suite suites))
    (let ((failed 0)
          (exn   #f)
          (out        (open-output-string))
          (err        (open-output-string))
          (suite-name (rackunit-test-suite-name suite)))
      (try (parameterize ((current-output-port out)
                          (current-error-port  err))
             (set! failed (run-tests suite 'normal)))
           (catch ((exn? (lambda (v)
                           (set! failed (add1 failed))
                           (set! exn v)))))
           (finally
            (display (~a suite-name ": " #:min-width 25 #:right-pad-string " "))
            (unless (= 0 failed) (displayln (format "FAILED\n")))
            (let ((out-str (get-output-string out))
                  (err-str (get-output-string err)))
              (when (non-empty-string? out-str)
                (display out-str))
              (when (non-empty-string? err-str)
                (display err-str)))))

      (when exn (raise exn))
      (unless (= 0 failed)
        (error (format "some tests in ~s suite failed" suite-name))))))

;; syntax & symbolic

(define (*->symbol v)
  (match v
    ((? symbol?  v) v)
    ((? number?  v) v)
    ((? string?  v) (string->symbol v))
    ((? keyword? v) (*->symbol (keyword->string v)))
    ((? syntax?  v) (*->symbol (syntax->datum v)))))

(define (*->string v)
  (match v
    ((? symbol?  v) (symbol->string v))
    ((? string?  v)  v)
    ((? number?  v) (number->string v))
    ((? false?   v)  "")))

(define-for-syntax (symbolic-identifier=? id1 id2)
  (eq? (syntax->datum id1)
       (syntax->datum id2)))

;; runtime

(define-syntax (try stx)
  (syntax-case* stx (catch finally) symbolic-identifier=?
		((_ e (catch ((pred handler) ...)) (finally e0 e1 ...))
		(syntax (dynamic-wind void
				      (lambda () (with-handlers ((pred handler) ...) e))
				      (lambda () e0 e1 ...))))
		((_ e (catch ((pred handler) ...)))
		(syntax (with-handlers ((pred handler) ...) e)))
		((_ e (finally e0 e1 ...))
		(syntax (dynamic-wind void
				      (lambda () e)
				      (lambda () e0 e1 ...))))))

(define (execute path . args)
  (let*-values (((trim)
                 (lambda (v) (string-trim
                              v "\n"
                              #:left?  #f
                              #:right? #t)))
                ((p out in err)
                 (apply subprocess
                        #f #f #f
                        (find-executable-path path)
                        args)))
    (try (begin
           (close-output-port in)
           (subprocess-wait p)
           (let ((code (subprocess-status p)))
             (unless (= code 0)
               (error (format "command ~a exited with ~a code: ~a"
                              path code (trim (port->string err))))))
           (trim (port->string out)))
         (catch ((exn? (lambda (exn)
                         (error (format "failed to run the command `~a`: ~a"
                                        (cons path args)
                                        (exn-message exn)))))))
         (finally
          (close-input-port out)
          (close-input-port err)))))

;; functional

;; (define (identity v) v)

(define ((partial-left f . xs) . xxs)
  (apply f (append xs xxs)))

(define ((partial-right f . xs) . xxs)
  (apply f (append xxs xs)))

(define partial partial-left)

;; (define-syntax (compose stx)
;;   (syntax-case stx ()
;;     ((_)            (syntax (lambda (v) v)))
;;     ((_ f)          (syntax f))
;;     ((_ f g h ...)  (syntax (lambda (v) (f ((compose g h ...) v)))))))

;;

(define (insert-at l p v)
  (define-values (before after) (split-at l p))
  (append before (cons v after)))

(define (flatten xs)
  (if (list? xs)
      (for/fold ((acc null))
                ((x (in-list xs)))
        (set! acc (append acc
                          (if (list? x)
                              (flatten x)
                              (list x))))
        acc)
      xs))

(define (hash-set-cons! hash key v)
  (hash-set! hash key (cons v (hash-ref hash key null))))

(module+ test
  (require rackunit)

  (define (+* x y) (+ x y))

  (run-suites
   (list (test-suite "identity"
                     (check-equal? (identity 1) 1)
                     (check-equal? (identity (list 1 2 3)) (list 1 2 3)))
         (test-suite "partial-left"
                     (check-equal? ((partial-left +*) 1 2)  3)
                     (check-equal? ((partial-left +* 1) 2)  3)
                     (check-equal? ((partial-left +* 1 2))  3)
                     (check-equal? ((partial-left /) 1 2)   1/2)
                     (check-equal? ((partial-left / 1) 2)   1/2)
                     (check-equal? ((partial-left / 1 2))   1/2)
                     (check-equal? ((partial-left / 1 2) 3) 1/6))
         (test-suite "partial-right"
                     (check-equal? ((partial-right +*) 1 2)  3)
                     (check-equal? ((partial-right +* 1) 2)  3)
                     (check-equal? ((partial-right +* 1 2))  3)
                     (check-equal? ((partial-right /) 1 2)   1/2)
                     (check-equal? ((partial-right / 1) 2)   2/1)
                     (check-equal? ((partial-right / 1 2))   1/2)
                     (check-equal? ((partial-right / 1 2) 3) 3/2))
         (test-suite "compose"
                     (check-equal? ((compose (partial-right expt 2)
                                             add1
                                             abs)
                                    -2) ;; -2 -> 2 -> 3 -> 9 (bottom-up order)
                                   9))

         (test-suite "flatten"
                     (check-equal?
                      (flatten (list (cons 1 2) (list 3)
                                     (list (list (list (cons 1 2))))))
                      '((1 . 2) 3 (1 . 2))))

         (test-suite "hash-set-cons!"
                     (check-equal?
                      (let ((h (make-hasheq)))
                        (hash-set-cons! h 'foo 1)
                        h)
                      (make-hasheq (list (cons 'foo (list 1)))))
                     (check-equal?
                      (let ((h (make-hasheq)))
                        (hash-set-cons! h 'foo 1)
                        (hash-set-cons! h 'foo 2)
                        h)
                      (make-hasheq (list (cons 'foo (list 2 1)))))))))