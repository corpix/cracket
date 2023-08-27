#lang info
(define pkg-name "corpix-testbed")
(define collection "corpix")
(define deps '("base"))
(define build-deps '("rackunit-lib"
                     "gregor"
                     "threading"
                     "corpix-syntax"
                     "corpix-os"
                     "corpix-list"
                     "corpix-db"
                     "corpix-css"
                     "corpix-strings"
                     "corpix-struct"
                     "corpix-websocket"
                     "corpix-configuration"))
(define version "0.0")
(define pkg-authors '(corpix))
