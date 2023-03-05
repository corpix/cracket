#lang racket/base
(require racket/contract/base
         racket/match
         racket/function
         (prefix-in nsq: library/network/nsq))
(provide (struct-out queue)
         (contract-out
          (make-queue    (-> (cons/c string? exact-nonnegative-integer?) hash? queue?))
          (make-consumer (-> queue? symbol? exact-nonnegative-integer? procedure? procedure?))))

(struct queue (client))

(define (make-queue address (parameters (make-hasheq)))
  (queue (nsq:make-client
          (nsq:make-connection #:host (car address) #:port (cdr address))
          parameters)))

(define (make-consumer queue entity batch-size transformer)
  (define next
    (nsq:subscribe/manual (queue-client queue)
                          (symbol->string entity)
                          #:batch-size batch-size))
  (thunk
   (match (next)
     ((cons message finalize)
      (cons (transformer (nsq:message-body message)) finalize))
     ((? void? envelope) envelope))))
