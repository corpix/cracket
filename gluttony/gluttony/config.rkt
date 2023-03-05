#lang racket
(provide config)

(define queue
  (let ((parameters
         (make-hasheq `((default . ,(make-hasheq `((msg_timeout . 10000) (heartbeat_interval . 5000)))))))
        (consumer
         (make-hasheq `((default . ,(make-hasheq `((batch-size 1024))))))))
    (make-hasheq `((address    . ("127.0.0.1" . 4150))
                   (parameters . ,parameters)
                   (consumer   . ,consumer)))))

(define database
  (let ((batch (make-hasheq `((default . ,(make-hasheq `((size . 128) (flush-interval . 5000))))
                              (book    . ,(make-hasheq `((size . 1024))))))))
    (make-hasheq `((address . ("127.0.0.1" . 8123))
                   (batch   . ,batch)))))

(define config
  (make-hasheq `((logger   . ,(make-hasheq `((level . info))))
                 (runtime  . ,(make-hasheq `((memory-limit . ,(* 128 1024 1024)))))
                 (statsd   . ,(make-hasheq `((prefix . "gluttony."))))
                 (queue    . ,queue)
                 (database . ,database)
                 (entities . (trades book)))))
