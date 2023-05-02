#lang racket
(require "jsonrpc.rkt")

(define-jsonrpc (bar #:baz baz #:qux (qux "hello"))
  (displayln (list 'bar-called baz qux))
  (make-hash `((baz . ,baz) (qux . ,qux))))

;;

(define server (jsonrpc-websocket-serve "127.0.0.1" 9094))
(sleep 1) ;; FIXME: hmmm server started asyncly?
(define client (jsonrpc-websocket-client "ws://127.0.0.1:9094"))

(with-jsonrpc client (displayln (bar #:baz 1)))

(close-jsonrpc-server server)
(close-jsonrpc-client client)
