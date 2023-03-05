#lang racket/base
(require racket/contract)

(provide (struct-out connection)
         (contract-out
          (make-connection (->* () (#:host string? #:port exact-nonnegative-integer?)
                                connection?))))

(struct connection (host port))

(define (make-connection
         #:host (host "127.0.0.1")
         #:port (port 8123))
  (connection host port))
