#lang racket/base
(require racket/contract
         "constant.rkt")

(provide (all-defined-out))

(struct exn:fail:user:nsq:protocol exn:fail:user ())

(struct pump (thread stop-evt))
(struct channels (message))
(struct client (connection protocol channels))
(struct command (name arguments body) #:prefab)
(struct frame (type body) #:prefab)
(struct connection (in out))
(struct message (timestamp attempts id body) #:prefab)

(define (parameter? key)
  (if (memq key parameters) #t #f))

(define parameters-hash? (and/c (hash/c parameter? any/c #:flat? #t)))

(define (topic-name? name)
  (and (string? name)
       (regexp-match #rx"^[a-zA-Z0-9._-]+$" name)
       (> (string-length name) 1)
       (<= (string-length name) 64)))

