#lang racket/base
(require racket/contract/base
         racket/match
         racket/math
         library/structure/pair
         (prefix-in bitfinex: library/network/bitfinex))

(provide (contract-out
          (make-transformer (-> symbol? symbol? symbol? (cons/c string? string?) procedure?))))

(define (make-transformer exchange-name entity market pair)
  (define exchange-name/string (symbol->string exchange-name))
  (define market/string (symbol->string market))
  (define pair/list (pair->list pair))
  (define (transform value)
    (match value
      ((bitfinex:trading-trades-message id timestamp amount price)
       (list
        id (exact-round (/ timestamp 1000))
        amount price
        exchange-name/string market/string
        pair/list))
      ((bitfinex:trading-book-message price count amount)
       (list
        (current-seconds) price count amount
        exchange-name/string market/string
        pair/list))))
  (match entity
    ('trades transform)
    ('book   transform)))


