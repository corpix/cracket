#lang racket
(require racket/syntax
         racket/syntax-srcloc
         syntax/strip-context
         syntax/parse
         syntax/parse/experimental/template)
(provide (all-from-out racket/syntax
                       racket/syntax-srcloc
                       syntax/strip-context
                       syntax/parse
                       syntax/parse/experimental/template)
         syntax-srcdir)

(define (syntax-srcdir stx)
  (path-only (normalize-path (srcloc-source (syntax-srcloc stx)))))
