#lang racket
(require racket/syntax
         racket/syntax-srcloc
         syntax/strip-context
         syntax/parse)
(provide (all-from-out racket/syntax
                       racket/syntax-srcloc
                       syntax/strip-context
                       syntax/parse)
         syntax-srcdir)

(define (syntax-srcdir stx)
  (path-only (normalize-path (srcloc-source (syntax-srcloc stx)))))
