#lang info
(define pkg-name "corpix-gluttony")
(define collection "corpix")
(define deps '("racket" "corpix/nsq" "corpix/clickhouse"))
(define build-deps '("rackunit-lib"))
(define version "0.0")
(define pkg-authors '(corpix))
(define racket-launcher-names '("gluttony"))
(define racket-launcher-libraries '("gluttony/gluttony.rkt"))
