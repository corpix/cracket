#lang racket
(require "event.rkt")
(provide batch-writer%
         make-interval-batch-writer)

(define batch-writer%
  (class object%
    (super-new)
    (init-field flush (buffer-size 128))
    (define semaphore (make-semaphore 1))
    (define counter 0)
    (define buffer (make-vector buffer-size))
    (define evt (make-notify-evt))

    (define (flush!/maybe)
      (and (> counter 0)
           (begin
             (flush (vector-take buffer counter))
             (notify! evt)
             (set! evt (make-notify-evt))
             (set! counter 0))
           #t))

    (define/public (flush!)
      (call-with-semaphore semaphore flush!/maybe))

    (define/public (flush-evt)
      (call-with-semaphore semaphore (thunk evt)))

    (define/public (append! record)
      (call-with-semaphore
       semaphore
       (lambda ()
         (vector-set! buffer counter record)
         (set! counter (add1 counter))
         (and (= counter buffer-size) (flush!/maybe)))))))

(define (make-interval-batch-writer batch time)
  (thread (thunk
           (let loop ()
             (define timer-evt (alarm-evt (+ (current-milliseconds) time)))
             (define v (sync timer-evt (send batch flush-evt) (thread-receive-evt)))
             (cond ((notify-evt? v)   (loop))
                   ((eq? v timer-evt) (send batch flush!) (loop)))))))

(module+ test
  (require rackunit
           racket/async-channel)

  (test-case "batch-writer%"
    (test-case "append!+flush!"
      (let* ((chan (make-async-channel 1))
             (batch (new batch-writer%
                         (flush (lambda (vec) (async-channel-put chan vec)))
                         (buffer-size 3))))
        (test-case "single"
          (check-false (send batch append! "hello"))
          (check-true (send batch flush!))
          (check-equal? (sync/timeout 3 chan) (vector "hello")))
        (test-case "multiple"
          (check-false (send batch append! "hello to"))
          (check-false (send batch append! "you"))
          (check-true (send batch flush!))
          (check-equal? (sync/timeout 3 chan) (vector "hello to" "you")))
        (test-case "buffer-size"
          (check-false (send batch append! "hello"))
          (check-false (send batch append! "world"))
          (check-true (send batch append! "!"))
          (check-equal? (sync/timeout 3 chan) (vector "hello" "world" "!")))
        (test-case "empty"
          (check-false (send batch flush!))
          (check-equal? (sync/timeout 0 chan) #f))))
    (test-case "flush-evt"
      (let* ((batch (new batch-writer% (flush void)))
             (done (send batch flush-evt)))
        (send batch append! "hello")
        (send batch flush!)
        (check-true (notify-evt? (sync/timeout 3 done)))
        (check-equal? (sync/timeout 0 (send batch flush-evt)) #f)))
    (test-case "interval-batch-writer"
      (let* ((chan (make-async-channel 1))
             (batch (new batch-writer% (flush (lambda (vec) (async-channel-put chan vec)))))
             (timer (make-interval-batch-writer batch 100)))
        (send batch append! "hello")
        (send batch append! "you")
        (check-true (notify-evt? (sync/timeout 3 (send batch flush-evt))))
        (thread-send timer (void))
        (check-equal? (sync/timeout 3 chan) (vector "hello" "you"))))))
