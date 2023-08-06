#lang racket
(require racket/generator)
(provide (all-from-out racket/generator)
         generator->list)

(define (generator->list generator (stop (void)))
  (for/list ((v (in-producer generator stop))) v))
