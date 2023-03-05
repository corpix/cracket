#lang racket/base
(require racket/contract/base
         racket/class
         racket/function
         racket/match
         library/logic
         (prefix-in bitfinex: library/network/bitfinex)
         "transformer.rkt")

(provide (struct-out exchange)
         (contract-out
          (make-exchange   (-> symbol? exchange?))
          (make-subscriber (-> exchange? symbol? symbol? (cons/c string? string?)
                               (values exact-nonnegative-integer? procedure?)))))

(struct exchange (name client))

(define (make-exchange name)
  (exchange
   name
   (match name
     ('bitfinex (bitfinex:make-connection #:when-eof (λ (t) (send t close)))))))

(define (make-subscriber exchange entity market pair)
  (let-values
      (((id next)
        ((match (exchange-name exchange)
           ('bitfinex
            (match entity
              ('trades (thunk (bitfinex:subscribe/trades (exchange-client exchange) market pair)))
              ('book   (thunk (bitfinex:subscribe/book   (exchange-client exchange) market pair))))))))
       ((transform) (make-transformer (exchange-name exchange) entity market pair)))
    (values
     id
     (thunk
      (define message (next))
      (void/or message (transform message))))))
