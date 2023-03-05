#lang racket/base
(require "type.rkt")

(provide (all-defined-out))

(define (unmarshal-message buf)
  (let ((timestamp   (integer-bytes->integer (subbytes buf 0 8) #t #t))
        (attempts    (integer-bytes->integer (subbytes buf 8 10) #f #t))
        (id          (subbytes buf 10 26))
        (body        (subbytes buf 26)))
    (message timestamp attempts id body)))
