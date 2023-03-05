#lang racket
(require "bitfinex.rkt")

(parameterize ((current-custodian (make-custodian (current-custodian))))
  (displayln 'running)
  (custodian-limit-memory
   (current-custodian)
   (* 128 1024 1024))
  (let ((conn (make-bitfinex-connection)))
    (for ((pair '(("BTC" . "USD")
                  ("ETH" . "USD"))))
      (let-values (((subscription-id next) (bitfinex-subscribe/trades conn 'trading pair)))
        (thread (thunk
                 (let loop ()
                   (define message (next))
                   (when (not (void? message))
                     (displayln (list pair subscription-id message))
                     (loop))))))))
  (with-handlers ((exn:break? (thunk*
                               (displayln 'stopping)
                               (custodian-shutdown-all (current-custodian)))))
    (sync never-evt)))
