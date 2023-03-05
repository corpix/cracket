#lang racket/base
(require (for-syntax racket/base
                     racket/match
                     racket/syntax)
         racket/class
         racket/contract/base
         racket/function
         racket/generator
         racket/match
         library/runtime/event
         "client.rkt"
         "connection.rkt"
         "parameter.rkt"
         "statsd.rkt"
         "type.rkt")

(provide
 (all-from-out "client.rkt"
               "connection.rkt"
               "parameter.rkt")
 (contract-out
  (publish          (-> client? topic-name? bytes? void?))
  (subscribe        subscribe/c)
  (subscribe/manual subscribe/c))
 (struct-out message))

(define subscribe/c
  (->* (client? topic-name?)
       (#:channel    topic-name?
        #:batch-size exact-nonnegative-integer?)
       generator?))

(define (publish client topic body)
  (send (client-protocol client) publish topic body)
  (statsd:topic:count "publish" topic 1))

(define (subscribe/manual client topic
                   #:channel    (channel (default-channel))
                   #:batch-size (batch   100))
  (define protocol (client-protocol client))
  (define stream (channels-message (client-channels client)))
  (define semaphore (make-semaphore 1))
  (define remains batch)
  (define drain-evt (make-notify-evt))

  (send protocol subscribe topic channel)
  (statsd:topic-channel:count "subscribe" topic channel 1)

  (send protocol ready remains)
  (generator
   ()
   (let loop ()
     (define sync-time    (current-milliseconds))
     (define message      (sync stream drain-evt (send protocol done-evt)))
     (define receive-time (current-milliseconds))
     (statsd:topic-channel:timer "wait" topic channel (- receive-time sync-time))

     (match message
       ((? message?)
        (statsd:topic-channel:count "messages" topic channel 1)
        (yield
         (cons message
               (thunk
                (handle-evt
                 (send protocol finalize (message-id message))
                 (λ (result)
                   (begin0 result
                     (call-with-semaphore
                      semaphore
                      (λ ()
                        (set! remains (sub1 remains))
                        (when (= remains 0) (notify! drain-evt))))
                     (statsd:topic-channel:timer "finalize" topic channel
                                                 (- (current-milliseconds) receive-time))))))))
        (loop))
       ((? notify-evt?)
        (statsd:topic-channel:count "drain" topic channel 1)
        (call-with-semaphore
         semaphore
         (λ ()
           (set! remains batch)
           (set! drain-evt (make-notify-evt))))
        (send protocol ready batch)
        (loop))
       ((? void?) message)))))

(define (subscribe client topic
                   #:channel    (channel (default-channel))
                   #:batch-size (batch   100))
  (define protocol (client-protocol client))
  (define stream (channels-message (client-channels client)))
  (send protocol subscribe topic channel)
  (statsd:topic-channel:count "subscribe" topic channel 1)
  (generator
   ()
   (let loop ((remains 0))
     (cond
       ((sync/timeout 0 (send protocol done-evt)) (void))
       ((= remains 0)
        (statsd:topic-channel:count "drain" topic channel 1)
        (send protocol ready batch)
        (loop batch))
       (else
        (define sync-time    (current-milliseconds))
        (define message      (sync stream))
        (define receive-time (current-milliseconds))

        (statsd:topic-channel:timer "wait"     topic channel (- receive-time sync-time))
        (statsd:topic-channel:count "messages" topic channel 1)

        (yield message)
        (send protocol finalize (message-id message))
        (statsd:topic-channel:timer "finalize" topic channel
                                    (- (current-milliseconds) receive-time))
        (loop (- remains 1)))))))
