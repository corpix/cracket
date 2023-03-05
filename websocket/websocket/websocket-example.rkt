#lang racket

(module+ main
  (require net/url
           "websocket.rkt")

  (define current-port (make-parameter 8081))
  (define current-address (make-parameter "127.0.0.1"))

  (define (server)
    (define (connection-handler connection state)
      (let loop ()
        (sync (handle-evt connection
                          (thunk*
                           (let ((message (websocket-receive connection #:payload-type 'text)))
                             (unless (eof-object? message)
                               (cond
                                 ((equal? message "stop")
                                  (websocket-send connection "stop triggered, closing..."))
                                 (else
                                  (websocket-send connection (format "message: ~a" message))
                                  (loop)))))))))
      (websocket-close connection))

    (displayln (format "listening on ws://~a:~a" (current-address) (current-port)))
    (websocket-serve #:listen-ip (current-address)
              #:port (current-port)
              connection-handler))

  (define (client)
    (define (receive-displayln c)
      (displayln (format "server says: ~a" (websocket-receive c)))
      (flush-output))

    (define (do-connection)
      (displayln (format "connecting to ws://~a:~a" (current-address) (current-port)))
      (define connection
        (websocket-connect "ws://127.0.0.1:8081/test?foo=bar"))
      (websocket-send connection "hello world")
      (receive-displayln connection)
      (websocket-send connection "stop")
      (receive-displayln connection)
      (websocket-close connection))

    (do-connection))

  ;;

  (define stop (server))

  (sleep 1)
  (client)
  ;; (with-handlers ((exn:break? (thunk*
  ;;                              (displayln "stopping websocket server")
  ;;                              (stop))))
  ;;   (sync never-evt))
  )
