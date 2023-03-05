#lang racket/base
(require (prefix-in statsd: library/network/statsd)
         racket/async-channel
         racket/class
         racket/function
         racket/hash
         racket/match
         racket/port
         "command.rkt"
         "constant.rkt"
         "frame.rkt"
         "io.rkt"
         "message.rkt"
         "parameter.rkt"
         "type.rkt")

(provide protocol%)

(define protocol%
  (class object%
    (super-new)
    (init-field connection parameters channels)

    (define await-reply null)
    (define last #f)
    (define semaphore (make-semaphore 1))

    (define in   (connection-in connection))
    (define out  (connection-out connection))
    (define pump (thread (thunk (loop))))

    (define/private (write command)
      (define buf (command->bytes command))
      (call-with-semaphore
       semaphore
       (lambda ()
         (set! last command)
         (write-bytes/flush buf out)
         (if (case (command-name command)
               ((identify pub sub cls) #t)
               ((rdy fin nop)          #f))
             (let ((evt (make-async-channel 1)))
               (set! await-reply (cons evt await-reply))
               evt)
             (handle-evt always-evt (lambda (_) #t))))))
    (define/private (reply (v #t))
      (call-with-semaphore
       semaphore
       (lambda ()
         (when (null? await-reply)
           (error (format "unexpected reply: ~a, last command was: ~a" v last)))
         (let ((ch (car await-reply)))
           (async-channel-put ch v)
           (set! await-reply (cdr await-reply))))))

    (sync (handshake))

    (define/private (loop)
      (match (sync (thread-receive-evt) (read-evt))
        ((frame 'message body)
         (async-channel-put (channels-message channels)
                            (unmarshal-message body))
         (loop))
        ((frame 'response #"_heartbeat_") (heartbeat) (loop))
        ((frame 'response #"OK") (reply) (loop))
        ((frame 'error body)
         (reply
          (exn:fail:user:nsq:protocol
           (format "protocol error: ~a, last command was: ~a"
                   (bytes->string/utf-8 body) last)
           (current-continuation-marks))))
        ((? eof-object?) (void))
        ((frame 'response #"CLOSE_WAIT") (reply))
        ((app (lambda (v) (eq? v (thread-receive-evt))) #t) (void))))
    (define/private (handshake)
      (write-bytes/flush magic out)
      (write
       (command 'identify #f
                (let ((buf (make-hasheq)))
                  (hash-union! buf (default-parameters) parameters
                               #:combine/key (lambda (k v1 v2) v2))
                  buf))))

    (define/public (publish topic body)
      (write (command 'pub (list topic) body)))
    (define/public (subscribe topic channel)
      (write (command 'sub (list topic channel) #f)))
    (define/public (ready amt)
      (write (command 'rdy (list amt) #f)))
    (define/public (finalize id)
      (write (command 'fin (list id) #f)))
    (define/public (heartbeat)
      (begin0 (nop) (statsd:count "nsq.heartbeat" 1)))
    (define/public (nop)
      (write (command 'nop #f #f)))
    (define/public (close)
      (write (command 'cls #f #f)))
    (define/public (done-evt)
      (handle-evt pump void))
    (define/public (read-evt)
      (choice-evt (eof-evt in) (handle-evt in read-frame)))))
