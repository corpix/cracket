#lang racket
(require racket/contract/base)
(provide notify-evt?
         make-notify-evt
         (contract-out
          (notify!  (-> notify-evt? void?))
          (sync/all (->* () #:rest (listof evt?) list?))))

(struct notify-evt
  (semaphore evt)
  #:property prop:evt (struct-field-index evt))

(define (make-notify-evt)
  (define semaphore (make-semaphore 0))
  (define (handle semaphore)
    ;; FIXME: could we do better?
    ;; result of evt should be constant
    (handle-evt (semaphore-peek-evt semaphore)
                (λ (_) (notify-evt semaphore (handle semaphore)))))
  (notify-evt semaphore (handle semaphore)))

(define (notify! evt)
  (let ((semaphore (notify-evt-semaphore evt)))
    (semaphore-try-wait? semaphore)
    (semaphore-post semaphore)))

(define (sync/all . evts)
  (let loop ((acc null) (n (length evts)) (evts evts))
    (if (= n 0)
        (reverse acc)
        (loop (cons (sync (car evts)) acc)
              (sub1 n)
              (cdr evts)))))

(module+ test
  (require rackunit
           racket/function)

  (test-case "notify!"
    (let* ((evt (make-notify-evt))
           (t   (thread (thunk (notify! evt)))))
      (check-true (notify-evt? (sync/timeout 1 evt)))
      (check-true (notify-evt? (sync/timeout 1 evt)))
      (check-false (not (sync/timeout 5 t)))))

  (test-case "sync/all"
    (let ((port (open-input-string "hello")))
      (check-equal? (sync/all always-evt port)
                    (list always-evt port)))))
