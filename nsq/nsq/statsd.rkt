#lang racket/base
(require (for-syntax racket/base
                     racket/syntax)
         (prefix-in statsd: library/network/statsd))

(provide statsd:topic:count
         statsd:topic:timer
         statsd:topic-channel:count
         statsd:topic-channel:timer)

(define-syntax (define/statsd:topic stx)
  (syntax-case stx ()
    ((_ metric)
     (with-syntax ((name      (format-id #'metric "statsd:topic:~a" #'metric))
                   (metric-fn (format-id #'metric "statsd:~a" #'metric)))
       #'(define (name event topic n)
           (metric-fn (format "nsq.topic.~a" event) n)
           (metric-fn (format "nsq.topic.~a.~a" topic event) n))))))

(define-syntax (define/statsd:topic-channel stx)
  (syntax-case stx ()
    ((_ metric)
     (with-syntax ((name      (format-id #'metric "statsd:topic-channel:~a" #'metric))
                   (metric-fn (format-id #'metric "statsd:~a" #'metric)))
       #'(define (name event topic channel n)
           (metric-fn (format "nsq.topic.~a" event) n)
           (metric-fn (format "nsq.topic.~a.~a" topic event) n)
           (metric-fn (format "nsq.topic.~a.channel.~a.~a" topic channel event) n))))))

(define/statsd:topic count)
(define/statsd:topic timer)
(define/statsd:topic-channel count)
(define/statsd:topic-channel timer)
