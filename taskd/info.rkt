#lang info
(define pkg-name "corpix-taskd")
(define collection "corpix")
(define deps '("racket" "corpix-task" "corpix-prometheus" "corpix-json"))
(define build-deps '("rackunit-lib"))
(define version "0.0")
(define pkg-authors '(corpix))
(define racket-launcher-names '("taskd"))
(define racket-launcher-libraries '("taskd/taskd.rkt"))
