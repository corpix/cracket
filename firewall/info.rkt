#lang info
(define collection "corpix")
(define deps '("racket" "net-ip"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/firewall.scrbl" ())))
(define pkg-desc "Rule based firewall")
(define version "0.0")
(define pkg-authors '(corpix))
(define license '(Unlicense))
