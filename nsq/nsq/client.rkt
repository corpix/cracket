#lang racket/base
(require
         racket/async-channel
         racket/class
         racket/contract
         "parameter.rkt"
         "protocol.rkt"
         "type.rkt")

(provide (contract-out
          (make-client (->* (connection?)
                            (parameters-hash?
                             #:channel-backlog exact-nonnegative-integer?)
                            client?))
          (close (->* (client?)
                      (#:timeout exact-nonnegative-integer?)
                      void?))))

(define (make-client connection (parameters (default-parameters))
                     #:channel-backlog (backlog 50))
  (let* ((channels (channels (make-async-channel backlog)))
         (protocol (new protocol%
                        (connection    connection)
                        (parameters    parameters)
                        (channels      channels))))
    (client connection protocol channels)))

(define (close client #:timeout (timeout 5))
  (let ((protocol   (client-protocol client))
        (connection (client-connection client)))
    (send protocol close)
    (sync/timeout timeout (send protocol done-evt))
    (close-input-port (connection-in connection))
    (close-output-port (connection-out connection))))
