#lang racket
(require racket/contract/base
         racket/function
         library/encoding/json
         (prefix-in nsq: library/network/nsq))
(provide (struct-out queue)
         (contract-out
          (make-queue    (-> (cons/c string? exact-nonnegative-integer?) hash? queue?))
          (make-producer (-> queue? symbol? procedure?))))

(struct queue (client))

(define (make-queue address (parameters (make-hasheq)))
  (queue (nsq:make-client
          (nsq:make-connection #:host (car address) #:port (cdr address))
          parameters)))

(define (make-producer queue entity)
  (let ((client (queue-client queue))
        (topic (symbol->string entity)))
    (λ (message)
      (nsq:publish client topic
                   (json->bytes message)))))
