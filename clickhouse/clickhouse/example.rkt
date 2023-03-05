#lang racket/base
(require racket/class
         library/iterator/generator
         "main.rkt")

(define conn (make-connection))

(void (generator->list (query conn "create table if not exists default.test (a UInt8, b UInt8) engine = Memory")))

(define (flush vec)
  (generator->list (query conn (insert #:into test #:rows ,vec))))

(define batch
  (new batch% (flush flush) (buffer-size 4096)))

(let loop ((a 0) (b 1))
  (when (<= b 1000)
    (begin
      (send batch append! (list a b))
      (loop (add1 a) (add1 b)))))

(void (send batch flush!))

(generator->list (parameterize ((schema '(UInt8))) (query conn (select (count) #:from test))))
