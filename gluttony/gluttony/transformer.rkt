#lang racket/base
(require racket/match
         library/encoding/json
         (prefix-in clickhouse: library/network/clickhouse))

(provide make-transformer)

(define (make-transformer entity)
  (match entity
    ('trades (thunk/trades))
    ('book   (thunk/book))))

(define (thunk/trades)
  (λ (body)
    (match-define
      (list id timestamp
            amount price
            exchange market (list from to))
      (bytes->json body))
    (list id timestamp
          amount price
          exchange market
          (clickhouse:function (tuple ,from ,to)))))

(define (thunk/book)
  (λ (body)
    (match-define
      (list timestamp price count amount exchange market (list from to))
      (bytes->json body))
    (list timestamp price count amount
          exchange market (clickhouse:function (tuple ,from ,to)))))

(module+ test
  (require rackunit)

  (test-case "thunk/trades"
    (check-equal?
     ((thunk/trades) (json->bytes (list "id" "timestamp" "amount" "price" "exchange" "market" (list "from" "to"))))
     '("id"
       "timestamp"
       "amount"
       "price"
       "exchange"
       "market"
       #s(sql:function
          tuple
          (#s(sql:expression #f #s(sql:parameter "from"))
           #s(sql:expression #f #s(sql:parameter "to")))))))

  (test-case "thunk/book"
    (check-equal?
     ((thunk/book) (json->bytes (list "timestamp" "price" "count" "amount" "exchange" "market" (list "from" "to"))))
     '("timestamp"
       "price"
       "count"
       "amount"
       "exchange"
       "market"
       #s(sql:function
          tuple
          (#s(sql:expression #f #s(sql:parameter "from"))
           #s(sql:expression #f #s(sql:parameter "to"))))))))
