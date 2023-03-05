#lang racket
(provide config)

(define queue
  (let ((parameters (make-hasheq `((default . ,(make-hasheq `((msg_timeout . 10000) (heartbeat_interval . 5000))))))))
    (make-hasheq `((address    .  ("127.0.0.1" . 4150))
                   (parameters . ,parameters)))))

(define config
  (make-hasheq `((logger    . ,(make-hasheq `((level . info))))
                 (runtime   . ,(make-hasheq `((memory-limit . ,(* 64 1024 1024)))))
                 (statsd    . ,(make-hasheq `((prefix . "pecker."))))
                 (queue     . ,queue)
                 (entities  . (trades))
                 (exchanges . (bitfinex))
                 (markets   . (trading))
                 (pairs     . (("BTC" . "USD")
                               ("EOS" . "USD") ("EOS" . "BTC")
                               ("LTC" . "USD") ("LTC" . "BTC")
                               ("ETH" . "USD") ("ETH" . "BTC"))))))
