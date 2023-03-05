#lang racket/base
(provide (all-defined-out))

(define (read-bytes/strict amt in)
  (let ((buf (read-bytes amt in)))
    (cond
      ((eof-object? buf)
       (raise (exn:fail:user
               "unexpected eof while reading from port"
               (current-continuation-marks))))
      ((< (bytes-length buf) amt)
       (raise (exn:fail:user
               (format "expected to read ~a bytes, but got only ~a" amt (bytes-length buf))
               (current-continuation-marks))))
      (else buf))))

(define (write-bytes/flush buf out)
  (write-bytes buf out)
  (flush-output out))

(module+ test
  (require rackunit)

  (test-case "read-bytes/strict"
    (check-equal? (read-bytes/strict 4 (open-input-bytes #"hello")) #"hell")
    (check-exn
     exn:fail:user?
     (lambda () (read-bytes/strict 4 (open-input-bytes #""))))
    (check-exn
     exn:fail:user?
     (lambda () (read-bytes/strict 5 (open-input-bytes #"wat"))))))
