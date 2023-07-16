#lang racket
(require racket/tcp)

(module+ main
  (require net/url
           "websocket.rkt")

  (define current-port (make-parameter 8081))
  (define current-address (make-parameter "127.0.0.1"))
  (define current-upstream-port (make-parameter 8082))
  (define current-upstream-address (make-parameter "127.0.0.1"))

  ;;

  (define (upstream)
    (displayln (format "listening on tcp://~a:~a" (current-upstream-address) (current-upstream-port)))
    (let ((cust (make-custodian)))
      (parameterize ((current-custodian cust))
        (let* ((listener (tcp-listen (current-upstream-port) 16 #t (current-upstream-address)))
               (worker (thread (thunk (let loop ()
                                        (parameterize ((current-custodian (make-custodian)))
                                          (let-values (((in out) (tcp-accept listener)))
                                            (sync (thread (thunk (let loop ()
                                                                   (let ((message (read-line in)))
                                                                     (unless (eof-object? message)
                                                                       (cond
                                                                         ((equal? "stop" message)
                                                                          (displayln "stop triggered, exiting..."))
                                                                         (else
                                                                          (displayln (format "got message: ~a" message))
                                                                          (loop))))))))
                                                  (thread (thunk
                                                           (let loop ((n 0))
                                                             (write-bytes #"ping" out)
                                                             (flush-output out)
                                                             (sleep 1)
                                                             (when (< n 5)
                                                               (loop (+ 1 n)))))))
                                            (custodian-shutdown-all (current-custodian)))))))))
          (thunk (custodian-shutdown-all cust))))))

  (define (server handler)
    (displayln (format "listening on ws://~a:~a" (current-address) (current-port)))
    (websocket-serve #:listen-ip (current-address)
                     #:port (current-port)
                     handler))


  (define (client)
    (define (receive-displayln connection)
      (displayln (format "server says: ~a"
                         (websocket-receive connection
                                            #:payload-type websocket-payload-binary)))
      (flush-output))

    (define (do-connection)
      (displayln (format "connecting to ws://~a:~a" (current-address) (current-port)))
      (define connection
        (websocket-connect "ws://127.0.0.1:8081/test?foo=bar"))
      (websocket-send connection "hello world\n"
                      #:payload-type websocket-payload-binary)
      (receive-displayln connection)
      (websocket-send connection "stop\n"
                      #:payload-type websocket-payload-binary)
      (receive-displayln connection)
      (websocket-close connection))
    (do-connection))

  ;;

  (define stop-upstream (upstream))
  (define stop (server (make-websocket-pipe-connection-handler
                        (current-upstream-address)
                        (current-upstream-port))))

  (sleep 1)
  (client)
  ;; (with-handlers ((exn:break? (thunk*
  ;;                              (displayln "stopping websocket server")
  ;;                              (stop))))
  ;;   (sync never-evt))
  )
