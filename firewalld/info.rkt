#lang info
(define collection "corpix")
(define deps '("racket" "net-ip"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/firewalld.scrbl" ())))
(define pkg-desc "Rule based firewall daemon")
(define version "0.0")
(define pkg-authors '(corpix))
(define license '(Unlicense))
(define racket-launcher-names '("firewalld"))
(define racket-launcher-libraries '("firewalld.rkt"))