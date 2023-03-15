#lang racket
(require racket/contract)
(provide current-clickhouse-connection
         (except-out (struct-out clickhouse-connection) -make-clickhouse-connection)
         (contract-out
          (make-clickhouse-connection
           (->* () (#:host string? #:port exact-nonnegative-integer?) clickhouse-connection?))))

(define current-clickhouse-connection (make-parameter #f))

(define-struct clickhouse-connection
  (host port)
  #:constructor-name -make-clickhouse-connection)

(define (make-clickhouse-connection
         #:host (host "127.0.0.1")
         #:port (port 8123))
  (-make-clickhouse-connection host port))
