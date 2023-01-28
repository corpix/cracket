#lang info
(define collection "corpix")
(define deps '("racket"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/time.scrbl" ())))
(define pkg-desc "Time parsing and serialization library")
(define version "0.0")
(define pkg-authors '(corpix))
(define license '(Unlicense))
