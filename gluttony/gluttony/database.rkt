#lang racket/base
(require racket/class
         racket/contract/base
         racket/match
         racket/vector
         library/iterator/generator
         library/runtime/supervisor
         (prefix-in clickhouse: library/network/clickhouse)
         (prefix-in statsd: library/network/statsd))
(provide (struct-out database)
         (contract-out
          (trades/init                (->  database? list?))
          (make-database              (-> (cons/c string? exact-nonnegative-integer?) database?))
          (make-batch-writer          (->  database? symbol? exact-nonnegative-integer? (is-a?/c clickhouse:batch-writer%)))
          (make-interval-batch-writer (-> (is-a?/c clickhouse:batch-writer%) exact-nonnegative-integer? thread?)))
         trades/schema)

(struct database (client))

(define trades/schema
  (clickhouse:columns
   (id         UInt64)
   (timestamp  DateTime)
   (amount     Float32)
   (price      Float32)
   (exchange  (Enum8 ("bitfinex" 0)))
   (market    (Enum8 ("trading" 0) ("funding" 1)))
   (pair      (Tuple (FixedString 3) (FixedString 3)))))

(define (trades/init database)
  (generator->list
   (clickhouse:query
    (database-client database)
    (clickhouse:create-table #:if-not-exists  trades
                             #:columns       ,trades/schema
                             #:engine        (MergeTree)
                             #:partition-by  (toYYYYMMDD timestamp)
                             #:order-by       timestamp))))

(define book/schema
  (clickhouse:columns
   (timestamp  DateTime)
   (price      Float32)
   (count      UInt32)
   (amount     Float32)
   (exchange  (Enum8 ("bitfinex" 0)))
   (market    (Enum8 ("trading" 0) ("funding" 1)))
   (pair      (Tuple (FixedString 3) (FixedString 3)))))

(define (book/init database)
  (generator->list
   (clickhouse:query
    (database-client database)
    (clickhouse:create-table #:if-not-exists  book
                             #:columns       ,book/schema
                             #:engine        (MergeTree)
                             #:partition-by  (toYYYYMMDD timestamp)
                             #:order-by       timestamp))))

(define (make-database address)
  (database
   (clickhouse:make-connection #:host (car address) #:port (cdr address))))

(define (make-batch-writer database entity size)
  (define client (database-client database))
  (define (flush vec)
    (parameterize ((statsd:prefix (format "~adatabase.batch.~a." (statsd:prefix) entity)))
      (statsd:with-timer
       "flush.time"
       (let ((statement (clickhouse:insert #:into ,entity #:rows ,(vector-map car vec))))
         (void (generator->list (clickhouse:query client statement)))))
      (statsd:with-timer "flush.finalize.time" (for ((pf (in-vector vec))) ((cdr pf))))
      (statsd:count "flush" 1)))

  ;; FIXME: it should not be here
  ((match entity
     ('trades trades/init)
     ('book   book/init))
   database)

  (new clickhouse:batch-writer%
       (buffer-size size)
       (flush       flush)))

(define (make-interval-batch-writer batch interval)
  (clickhouse:make-interval-batch-writer batch interval))
