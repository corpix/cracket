#lang info
(define pkg-name "corpix-firewalld")
(define collection "corpix")
(define deps '("racket" "corpix-net"))
(define build-deps '("rackunit-lib"))
(define version "0.0")
(define pkg-authors '(corpix))
(define racket-launcher-names '("firewalld"))
(define racket-launcher-libraries '("firewalld/firewalld.rkt"))
