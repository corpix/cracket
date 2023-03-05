#lang racket/base
(require racket/match
         "io.rkt"
         "type.rkt")

(provide (all-defined-out))

(define (frame-id->type t)
  (match t
    (0 'response)
    (1 'error)
    (2 'message)
    (_ #f)))

(define (marshal-frame-size size)
  (integer->integer-bytes size 4 #f #t))

(define (unmarshal-frame-size size)
  (integer-bytes->integer size   #f #t))

(define (unmarshal-frame-type type)
  (frame-id->type (integer-bytes->integer type #f #t)))

(define (read-frame in)
  (define size (unmarshal-frame-size (read-bytes/strict 4 in)))
  (define body-size  (- size 4))
  (when (< body-size 0)
    (error (format "got frame with negative size: ~a" body-size)))
  (frame (unmarshal-frame-type (read-bytes/strict 4 in))
         (and (> body-size 0)
              (read-bytes/strict body-size in))))

(module+ test
  (require rackunit)

  (test-case "frame-id->type"
    (check-equal? (frame-id->type 0)   'response)
    (check-equal? (frame-id->type 1)   'error)
    (check-equal? (frame-id->type 2)   'message)
    (check-equal? (frame-id->type 666) #f))

  (test-case "marshal-frame-size"
    (check-equal? (marshal-frame-size (bytes-length #"hello"))
                  #"\0\0\0\5")
    (check-equal? (marshal-frame-size (bytes-length #"totally reckless"))
                  #"\0\0\0\20")))
