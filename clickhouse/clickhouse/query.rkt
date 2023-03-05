#lang racket/base

(require (prefix-in http: library/network/http)
         (prefix-in stats: (only-in library/network/statsd timer))
         racket/contract
         racket/generator
         racket/match
         racket/port
         racket/string
         "connection.rkt"
         "convert.rkt"
         "sql.rkt"
         "type.rkt")

(provide schema
         (struct-out exn:fail:user:clickhouse:query)
         (contract-out
          (query-raw     (-> connection? (or/c string? sql:statement?) generator?))
          (query-convert (-> generator? procedure? list? generator?))
          (query         (-> connection? (or/c string? sql:statement?) generator?))))

(define schema (make-parameter null))

(define (query-send! connection query)
  (http:request-send! (http:make-request
                       (connection-host connection)
                       (connection-port connection)
                       "/"
                       #:method 'post
                       #:query (list (cons 'query query))
                       #:headers (http:make-headers
                                  ('Connection   "keep-alive")
                                  ('Content-Type "application/x-www-form-urlencoded"))
                       #:body query)))

(define (query-raw connection statement)
  (let* ((start-time (current-milliseconds))
         (query (match statement
                  ((? string?)         statement)
                  ((? sql:statement?) (sql:statement->string statement))))
         (response (query-send! connection query))
         (status (http:response-status response))
         (in (http:response-body-reader response)))
    (when (not (equal? status 200))
      (define message (port->string in))
      (close-input-port in)
      (raise (exn:fail:user:clickhouse:query
              (format "query failed with: ~a" message)
              (current-continuation-marks)
              query
              status
              message)))
    (generator
     ()
     (let loop ()
       (define data (read-line in))
       (cond
         ((eof-object? data)
          (close-input-port in)
          (stats:timer "clickhouse.client.query-raw"
                       (- (current-milliseconds) start-time)))
         (else
          (yield (string-split data "\t"))
          (loop)))))))

(define (query-convert next transition schema)
  (generator
   ()
   (let loop ()
     (define data (next))
     (cond
       ((void? data) data)
       (else (yield
              (if (null? schema)
                  data
                  (map
                   (lambda (value type)
                     (convert type (transition type) value))
                   data schema)))
             (loop))))))

(define (query connection statement)
  (let ((generator (query-raw connection statement)))
    (query-convert generator transition (schema))))
