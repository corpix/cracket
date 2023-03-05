#lang racket/base
(require racket/match
         racket/syntax
	 syntax/parse
         (for-syntax racket/base
                     racket/match
                     racket/syntax
                     syntax/parse))

(provide (all-defined-out)
         (all-from-out racket/match)
         (for-syntax (all-defined-out)
                     (all-from-out racket/match)))

#|
see:
- https://srfi.schemers.org/srfi-200/srfi-200.html
- https://srfi.schemers.org/srfi-204/srfi-204.html
- http://synthcode.com/scheme/match.scm
- https://www.iro.umontreal.ca/~feeley/cours/ift6232/doc/pres2/practical-soft-type-system-for-scheme.pdf
|#

;;

(define-syntax (define-pattern stx)
  (syntax-case stx ()
    ((_ (name args ...) pat)
     (syntax
      (define-match-expander name
        (lambda (stx) (syntax-case stx ()
                        ((_ args ...)
                         (syntax pat)))))))))

;; (define-pattern (%test t)
;;   (or (? symbol?  t)
;;       (? keyword? t)
;;       (? string?  t)))

;; (match
;;  (list 'foo '#:hey 'baz "qux")
;;  ((list (%test t) ...) (list t)))

;; EXPERIMENTS

(define-pattern (%operator op operands ...)
  (list
   (? (lambda (v)
        (and
         (memq v '(+ - % * /
                     == != > < >= <=
                     ! not
                     && and
                     \|\| or
                     bitwise-and ;; | is a reader in racket, so | is problematic, not supporting & too, to make it more easy to remember until I find a more beautiful solution
                     bitwise-or
                     ^  bitwise-xor
                     << bitwise-left-shift
                     >> bitwise-right-shift))
         v))
      => op)
   operands ...))

(define-pattern (%nil expr)
  (? (lambda (v) (eq? v 'nil))
     expr))
(define-pattern (%bool expr)
  (? (lambda (v)
       (or (eq? v '#t)
           (eq? v '#t)))
     expr))
(define-pattern (%number expr)
  (? number? expr))
(define-pattern (%string expr)
  (? string? expr))
(define-pattern (%symbol expr)
  (? symbol? expr))
(define-pattern (%list expr)
  (? list? expr))

;; TODO: pattern => transformation
;; how to merge?

(define-pattern (%expr expr)
  (or
   (%nil    expr)
   (%bool   expr)
   (%symbol expr)
   (%number expr)
   (%string expr)
   (%list   expr)))

(define (match-expr expr)
  (match expr
    ()))

(define-pattern (%func-call fn args)
  (or
   ((%symbol fn) (%expr args))
   ((%expr   fn) (%expr args))))

(match '(hello (you nil (+ 1 1)))
  ((%expr e) e)
  (_ 'you-are-wrong-luke))
