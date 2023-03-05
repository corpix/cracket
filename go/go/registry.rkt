#lang racket/base
(require "parameter.rkt")
(provide make-registry
         registry-ref
         registry-has?
         registry-set!
         registry-remove!)

(define (make-registry)
  (make-hasheq))

(define (registry-ref name . default)
  (apply hash-ref (*registry*) name default))

(define (registry-has? name)
  (and (registry-ref name #f) #t))

(define (registry-set! name package)
  (hash-set! (*registry*) name package))

(define (registry-remove! name)
  (hash-remove! (*registry*) name))
