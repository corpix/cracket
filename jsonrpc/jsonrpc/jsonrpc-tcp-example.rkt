#lang racket
(require "jsonrpc.rkt")

(define-jsonrpc (huh #:baz baz #:qux (qux "hello"))
  #:method bar
  (displayln (list 'bar-called baz qux))
  (make-hash `((baz . ,baz) (qux . ,qux))))

;;

(define server (jsonrpc-tcp-serve "127.0.0.1" 9094))
(define client (jsonrpc-tcp-client "127.0.0.1" 9094))

(with-jsonrpc client (displayln (huh #:baz 1)))

(close-jsonrpc-server server)
(close-jsonrpc-client client)
