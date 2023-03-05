#lang racket/base

(require racket/function
         racket/match
         racket/string
         library/runtime/supervisor
         library/service
         library/structure/struct
         library/structure/pair
         library/structure/hash
         library/runtime/logger
         (prefix-in statsd: library/network/statsd)
         (prefix-in stat: library/runtime/stat)
         "config.rkt"
         "exchange.rkt"
         "producer.rkt")

(define (app config)
  (apply choice-evt
         (for/list ((exchange-name (hash-ref config 'exchanges)))
           ((let ((entities (hash-ref config 'entities))
                  (markets  (hash-ref config 'markets))
                  (pairs    (hash-ref config 'pairs)))
              (supervisor
               #:name exchange-name
               #:fails 10 #:interval 60
               (sleep 5) ;; FIXME: implement some sort of strategy in supervisor to sleep only in case of multiple fails
               (with-heartbeat (heartbeat #:interval 10 #:fouls 2)
                 (define exchange (make-exchange exchange-name))
                 (sync (for*/fold ((acc null))
                                  ((entity entities)
                                   (market markets)
                                   (pair   pairs))
                         (let*-values (((id next)    (make-subscriber exchange entity market pair))
                                       ((parameters) (config-union config '(queue parameters) entity))
                                       ((queue)      (make-queue (hash-ref* config '(queue address)) parameters))
                                       ((produce)    (make-producer queue entity))
                                       ((pair-name)  (string-downcase (string-join (pair->list pair) "-"))))
                           (log-info "created ~a subscription #~a on ~a ~a market ~a pair"
                                     entity id exchange-name market pair)
                           (thread
                            (thunk
                             (let loop ()
                               (define message (next))
                               (when (not (void? message))
                                 (heartbeat)
                                 (produce message)
                                 (statsd:count
                                  (format "message.~a.~a.~a.~a" exchange-name market entity pair-name))
                                 (loop)))))))))))))))

(parameterize ((statsd:connection (statsd:make-connection))
               (statsd:prefix     (hash-ref* config '(statsd prefix))))
  (stat:thread/metrics #:interval 2500)
  (parameterize ((current-logger       (setup-logger config 'pecker))
                 (current-thread-group (make-thread-group))
                 (current-custodian    (make-custodian)))
    (custodian-limit-memory
     (current-custodian)
     (hash-ref* config '(runtime memory-limit)))

    (let ((supervisor (app config)))
      (when (not (sync/timeout 10 supervisor))
        (log-info "started")
        (sync supervisor))
      (log-error "supervisor is dead, initiating shutdown")
      (exit 1))))
