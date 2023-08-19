#lang racket
(require corpix/http
         corpix/prometheus
         racket/contract
         corpix/generator
         "connection.rkt"
         "convert.rkt"
         "syntax.rkt"
         "type.rkt"
         "emit.rkt"
         (for-syntax racket/base
                     corpix/syntax
                     "syntax.rkt"))
(provide current-clickhouse-schema
         clickhouse-metric-query-time
         clickhouse
         (struct-out exn:fail:user:clickhouse:query)
         (contract-out
          (clickhouse-query-raw (-> clickhouse-connection? (or/c string? clickhouse-sql-statement?) generator?))
          (clickhouse-query-convert (-> generator? procedure? list? generator?))
          (clickhouse-query (-> clickhouse-connection? (or/c string? clickhouse-sql-statement?) generator?))
          (clickhouse-query! (-> clickhouse-connection? (or/c string? clickhouse-sql-statement?) void?))))

(define current-clickhouse-schema (make-parameter null))

(define clickhouse-metric-query-time
  (make-prometheus-metric-histogram 'clickhouse-query-time
                                    #:doc "Time spent sending and executing query in clickhouse"))

(define (query-send! connection sql)
  (http-request-send! (make-http-request
                       (clickhouse-connection-host connection)
                       (clickhouse-connection-port connection)
                       "/"
                       #:method 'post
                       #:headers '((connection . "keep-alive")
                                   (content-type . "application/x-www-form-urlencoded"))
                       #:body sql)))

(define (clickhouse-query-raw connection statement)
  (let* ((start-time (current-milliseconds))
         (sql (match statement
                ((? string?) statement)
                ((? clickhouse-sql-statement?) (clickhouse-emit-statement statement))))
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
          (void (prometheus-observe! clickhouse-metric-query-time
                                     (- (current-milliseconds) start-time)
                                     #:labels `((host . ,(clickhouse-connection-host connection))
                                                (port . ,(clickhouse-connection-port connection))))))
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
                     (clickhouse-convert type (transition type) value))
                   data schema)))
             (loop))))))

(define (clickhouse-query connection statement)
  (clickhouse-query-convert (clickhouse-query-raw connection statement)
                            clickhouse-transition
                            (current-clickhouse-schema)))

(define (clickhouse-query! connection statement)
  (void (generator->list (clickhouse-query connection statement))))

(define-syntax (clickhouse stx)
  (syntax-parse stx
    ((_ (~optional (~seq #:connection connection) #:defaults ((connection #'(current-clickhouse-connection))))
        (name rest ...))
     (with-syntax ((prefixed-name (format-id #'name "clickhouse-~a" #'name)))
       (syntax/loc stx
         (clickhouse-query connection (prefixed-name rest ...)))))
    ((_ (~optional (~seq #:connection connection) #:defaults ((connection #'(current-clickhouse-connection))))
        sql:string)
     (syntax/loc stx
       (clickhouse-query connection sql)))))
