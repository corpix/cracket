#lang racket
(require racket/os
         racket/promise
         ffi/unsafe)
(provide (all-from-out racket/os)
         getuid
         getpid
         current-uid
         current-pid)

(define (getuid)
  ((get-ffi-obj "getuid" #f (_fun -> _int))))

(define (getpid)
  ((get-ffi-obj "getpid" #f (_fun -> _int))))

(define (current-uid) (make-parameter (getuid)))
(define (current-pid) (make-parameter (getpid)))
