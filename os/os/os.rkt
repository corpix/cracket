#lang racket
(require racket/os
         racket/promise
         ffi/unsafe)
(provide (all-from-out racket/os)
         getuid)

(define (getuid)
  ((get-ffi-obj "getuid" #f (_fun -> _int))))

(define (current-uid) (make-parameter (getuid)))
