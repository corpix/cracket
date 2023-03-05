#lang racket
(require "supervisor.rkt")

(sync ((supervisor
        #:name 'hello #:fails 5 #:interval 30
        (with-heartbeat (hb #:interval 1 #:fouls 5)
          (let loop ((n 10))
            (displayln n)
            (displayln 'begin)
            (sleep 0.1)
            (hb)
            (displayln 'end)
            (when (> n 0) (loop (- n 1))))))))
