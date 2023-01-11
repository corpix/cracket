#lang info
(define collection "corpix")
(define deps '("racket"))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "rackunit-lib"
                     "corpix-http"))
(define scribblings '(("scribblings/telegram.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(corpix))
(define license '(Unlicense))
