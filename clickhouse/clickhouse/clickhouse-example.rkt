#lang racket
(require "clickhouse.rkt")

(define (generator->list generator)
  (for/list ((v (in-producer generator (void)))) v))
(parameterize ((current-clickhouse-connection (make-clickhouse-connection)))
  (void (generator->list (clickhouse "create table if not exists default.test (a UInt8, b UInt8) engine = Memory")))
  (define (flush vec)
    (generator->list (clickhouse (insert #:into test #:rows ,vec))))
  (define batch
    (new clickhouse-batch-writer% (flush flush) (buffer-size 4096)))
  (let loop ((a 0) (b 1))
    (when (<= b 1000)
      (begin
        (send batch append! (list a b))
        (loop (add1 a) (add1 b)))))
  (void (send batch flush!))
  (generator->list (parameterize ((current-clickhouse-schema '(UInt8)))
                     (clickhouse (select (count) #:from test)))))
