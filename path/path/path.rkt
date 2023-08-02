#lang racket
(require racket/path)
(provide (all-from-out racket/path)
         path-trim-prefix)


(define (path-trim-prefix path prefix)
  (apply build-path
         (let loop ((prefix (explode-path prefix))
                    (path (explode-path path)))
           (if (and (pair? prefix) (pair? path))
               (let ((curr-path (car path)))
                 (if (equal? (car prefix) curr-path)
                     (loop (cdr prefix) (cdr path))
                     (cons curr-path (loop (cdr prefix) (cdr path)))))
               path))))
