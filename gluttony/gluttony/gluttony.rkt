#lang racket/base

(require json
         (prefix-in clickhouse: library/network/clickhouse)
         (prefix-in statsd: library/network/statsd)
         racket/class
         racket/math
         racket/string
         racket/function
         racket/hash
         library/service
         library/runtime/logger
         library/runtime/stat
         library/structure/hash
         library/runtime/supervisor
         "config.rkt"
         "consumer.rkt"
         "database.rkt"
         "transformer.rkt")

(define (app config)
  (apply choice-evt
         (for/list ((entity (hash-ref config 'entities)))
           (let* ((parameters            (config-union config '(queue parameters) entity))
                  (consumer              (config-union config '(queue consumer) entity))
                  (batch-size            (hash-ref consumer 'batch-size))
                  (transformer           (make-transformer entity))
                  (batch                 (config-union config '(database batch) entity))
                  (batch-size            (hash-ref batch 'size))
                  (batch-interval        (hash-ref batch 'flush-interval)))
             ((supervisor
               #:name entity
               #:fails 10 #:interval 60
               (let* ((queue                 (make-queue (hash-ref* config '(queue address)) parameters))
                      (consume               (make-consumer queue entity batch-size transformer))
                      (database              (make-database (hash-ref* config '(database address))))
                      (batch-writer          (make-batch-writer database entity batch-size))
                      (interval-batch-writer (make-interval-batch-writer batch-writer batch-interval)))
                 (sync
                  interval-batch-writer
                  (thread (thunk (let loop () (send batch-writer append! (consume)) (loop))))))))))))

(parameterize ((current-logger       (setup-logger config 'gluttony))
               (current-thread-group (make-thread-group))
               (current-custodian    (make-custodian))
               (statsd:connection    (statsd:make-connection))
               (statsd:prefix        (hash-ref* config '(statsd prefix))))
  (thread/metrics #:interval 2500)
  (custodian-limit-memory
   (current-custodian)
   (hash-ref* config '(runtime memory-limit)))
  (let ((supervisor (app config)))
    (when (not (sync/timeout 10 supervisor))
      (log-info "started")
      (sync supervisor))
    (log-error "supervisor is dead, initiating shutdown")
    (exit 1)))
