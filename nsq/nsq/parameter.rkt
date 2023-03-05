#lang racket/base
(provide (all-defined-out))

(define default-parameters (make-parameter #hasheq((heartbeat_interval . 30000))))
(define default-channel    (make-parameter "racket-nsq"))

