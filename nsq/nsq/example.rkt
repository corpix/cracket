#lang racket/base
(require racket/function
         racket/match
         library/runtime/stat
         (prefix-in statsd: library/network/statsd)
         "main.rkt")

(parameterize ((statsd:connection (statsd:make-connection))
               (statsd:prefix "nsq-example."))
  (thread/metrics)
  (define conn (make-connection))
  (define cl (make-client
              conn (make-hasheq '((msg_timeout        . 60000)
                                  (heartbeat_interval . 5000)))))

  (thread (thunk (let loop ((n 0))
                   (publish cl "hello" (string->bytes/utf-8 (format "count ~s" n)))
                   (sleep (/ (random) 10))
                   (when (= 0 (modulo n 100)) (displayln (format "sent ~s" n)))
                   (loop (add1 n)))))

  (thread (thunk
           (define next (subscribe/manual cl "hello"))
           (let loop ((n 0))
             (define pair (next))
             (cond ((void? pair) (displayln "generator finished"))
                   (else
                    (match (sync ((cdr pair)))
                      ((? exn? e) (raise e))
                      (_ #t))
                    (when (= 0 (modulo n 100)) (displayln (format "handled ~s" n)))
                    (loop (add1 n))))))))


(sync never-evt)
