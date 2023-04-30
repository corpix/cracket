#lang racket

(module+ main
  (require net/url
           "websocket.rkt")

  (define current-port (make-parameter 8081))
  (define current-address (make-parameter "127.0.0.1"))

  ;;;; example without using ports
  ;; (define (server)
  ;;   (define (connection-handler connection state)
  ;;     (let loop ()
  ;;       (sync (handle-evt connection
  ;;                         (thunk*
  ;;                          (let ((message (websocket-receive connection #:payload-type 'text)))
  ;;                            (unless (eof-object? message)
  ;;                              (cond
  ;;                                ((equal? message "stop")
  ;;                                 (websocket-send connection "stop triggered, closing..."))
  ;;                                (else
  ;;                                 (websocket-send connection (format "message: ~a" message))
  ;;                                 (loop)))))))))
  ;;     (websocket-close connection))

  ;;   (displayln (format "listening on ws://~a:~a" (current-address) (current-port)))
  ;;   (websocket-serve #:listen-ip (current-address)
  ;;             #:port (current-port)
  ;;             connection-handler))

  ;; example using ports
  (define (server)
    (define (connection-handler connection state)
      (define in (make-websocket-input-port connection))
      (define out (make-websocket-output-port connection))
      (let loop ()
        (let ((message (read-line in)))
          (unless (eof-object? message)
            (cond
              ((equal? message "stop")
               (write-string "stop triggered, closing...\n" out))
              (else
               (write-string (format "message: ~a\n" message) out)
               (loop))))))
      (close-input-port in)
      (close-output-port out))

    (displayln (format "listening on ws://~a:~a" (current-address) (current-port)))
    (websocket-serve #:listen-ip (current-address)
                     #:port (current-port)
                     connection-handler))

  ;;;; example without using ports
  ;; (define (client)
  ;;   (define (receive-displayln connection)
  ;;     (displayln (format "server says: ~a" (websocket-receive connection)))
  ;;     (flush-output))

  ;;   (define (do-connection)
  ;;     (displayln (format "connecting to ws://~a:~a" (current-address) (current-port)))
  ;;     (define connection
  ;;       (websocket-connect "ws://127.0.0.1:8081/test?foo=bar"))
  ;;     (websocket-send connection "hello world\n")
  ;;     (receive-displayln connection)
  ;;     (websocket-send connection "stop\n")
  ;;     (receive-displayln connection)
  ;;     (websocket-close connection))

  ;;   (do-connection))

  ;; example using ports
  (define (client)
    (displayln (format "connecting to ws://~a:~a" (current-address) (current-port)))
    (define connection (websocket-connect "ws://127.0.0.1:8081/test?foo=bar"))
    (define in (make-websocket-input-port connection))
    (define out (make-websocket-output-port connection))
    (write-string "hello world\n" out)
    (displayln (format "server says: ~a" (read-line in)))
    (write-string "stop\n" out)
    (displayln (format "server says: ~a" (read-line in)))
    (close-input-port in)
    (close-output-port out))

  ;;

  (define stop (server))

  (sleep 1)
  (client)
  ;; (with-handlers ((exn:break? (thunk*
  ;;                              (displayln "stopping websocket server")
  ;;                              (stop))))
  ;;   (sync never-evt))
  )
