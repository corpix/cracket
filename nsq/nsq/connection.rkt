#lang racket/base
(require racket/contract/base
         racket/tcp
         "type.rkt")

(provide (contract-out
          (make-connection  (->* ()
                                 (#:host string?
                                  #:port exact-nonnegative-integer?)
                                 connection?))))

(define (make-connection #:host (host "localhost") #:port (port 4150))
  (define-values (in out) (tcp-connect host port))
  (connection in out))


