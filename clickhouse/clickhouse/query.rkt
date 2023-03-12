#lang racket
(require corpix/http
         corpix/prometheus
         racket/contract
         racket/generator
         "connection.rkt"
         "convert.rkt"
         "sql.rkt"
         "type.rkt")
(provide current-clickhouse-schema
         clickhouse-metric-query-time
         (struct-out exn:fail:user:clickhouse:query)
         (contract-out
          (clickhouse-query-raw     (-> connection? (or/c string? sql:statement?) generator?))
          (clickhouse-query-convert (-> generator? procedure? list? generator?))
          (clickhouse-query         (-> connection? (or/c string? sql:statement?) generator?))))

(define current-clickhouse-schema (make-parameter null))

(define clickhouse-metric-query-time
  (make-prometheus-metric-histogram 'clickhouse-query-time
                                #:doc "Time spent sending and executing query in clickhouse"))

(define (query-send! connection sql)
  (http-request-send! (make-http-request
                       (connection-host connection)
                       (connection-port connection)
                       "/"
                       #:method 'post
                       #:headers '((connection   . "keep-alive")
                                   (content-type . "application/x-www-form-urlencoded"))
                       #:body sql)))

(define (clickhouse-query-raw connection statement)
  (let* ((start-time (current-milliseconds))
         (sql (match statement
                  ((? string?)         statement)
                  ((? sql:statement?) (sql:statement->string statement))))
         (response (query-send! connection sql))
         (status (http-response-status response))
         (in (http-response-body-reader response)))
    (when (not (equal? status 200))
      (define message (port->string in))
      (close-input-port in)
      (raise (exn:fail:user:clickhouse:query
              (format "query failed with: ~a" message)
              (current-continuation-marks)
              sql
              status
              message)))
    (generator
     ()
     (let loop ()
       (define data (read-line in))
       (cond
         ((eof-object? data)
          (close-input-port in)
          (prometheus-observe! clickhouse-metric-query-time
                               (- (current-milliseconds) start-time)
                               #:labels `((host . ,(connection-host connection))
                                          (port . ,(connection-port connection)))))
         (else
          (yield (string-split data "\t"))
          (loop)))))))

(define (clickhouse-query-convert next transition (schema (current-clickhouse-schema)))
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

(define (clickhouse-query connection statement)
  (let ((generator (clickhouse-query-raw connection statement)))
    (clickhouse-query-convert generator transition (current-clickhouse-schema))))
