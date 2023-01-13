#lang racket
(provide bytes-index
         bytes-contains-any)

(define (bytes-index vec octet (start 0) (end (bytes-length vec)))
  (let loop ((index start))
    (cond
     ((>= index end) #f)
     ((= octet (bytes-ref vec index)) index)
     (else (loop (+ 1 index))))))

(define (bytes-contains-any vec octets)
  (define len (bytes-length octets))
  (cond
    ((= len 0) #f)
    (else (let loop ((n 0))
            (if (< n len)
                (let* ((octet (bytes-ref octets n))
                       (i (bytes-index vec octet)))
                  (if i
                      (cons i octet)
                      (loop (+ n 1))))
                #f)))))

(module+ test
  (require rackunit)
  (test-case "bytes-index"
    (check-equal? (bytes-index (bytes 1 2 3) 3)
                  2)
    (check-equal? (bytes-index (bytes 1 2 3) 10)
                  #f)
    (check-equal? (bytes-index (bytes 1 2 3) 1 1)
                  #f)
    (check-equal? (bytes-index (bytes 1 2 3) 3 0 1)
                  #f)
    (check-equal? (bytes-index (bytes 1 2 3) 3 0)
                  2)
    (check-exn exn? (thunk (bytes-index (bytes 1 2 3) 10 0 10))))
  (test-case "bytes-contains-any"
    (check-equal? (bytes-contains-any (bytes 1 2 3) (bytes 6 5 4))
                  #f)
    (check-equal? (bytes-contains-any (bytes) (bytes))
                  #f)
    (check-equal? (bytes-contains-any (bytes 1 2) (bytes 1))
                  (cons 0 1))
    (check-equal? (bytes-contains-any (bytes 1 2) (bytes 2))
                  (cons 1 2))
    (check-equal? (bytes-contains-any (bytes 1 2 2) (bytes 2))
                  (cons 1 2))
    (check-equal? (bytes-contains-any (bytes 1 2 2 1) (bytes 2 1))
                  (cons 1 2))))
