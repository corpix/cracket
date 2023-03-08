#lang racket
(require "task.rkt")

;; (with-handlers ((exn? (lambda (exn) (displayln (format "there was exception, but it was ignored: ~a" exn)))))
;;   (tasks (eval (define foo 1)
;;                (displayln foo))
;;          (eval (displayln 2))
;;          (shell "exit 1")
;;          (shell "echo hello")))


;; TODO:
;; - threading macro

;; (tasks (concurrent (chain (concurrent (eval (sleep 30))
;;                                       (shell "sleep 3 && echo running nc && nc -l 43489"))
;;                           (timeout 7 _)
;;                           (try _ #:except (eval (displayln 'ooooops))))
;;                    ;; (try (timeout 7 (shell "sleep 3 && echo running nc && nc -l 43489")))
;;                    (sequential (shell "whoami")
;;                                (shell "pwd" #:cwd "/tmp")
;;                                (port 'tcp 43489 (sequential (eval (displayln "port opened, continuing"))
;;                                                             (shell "uname -a"))))))

;; (with-handlers ((exn? (lambda (exn) (displayln (format "there was exception, but it was ignored: ~a" exn)))))
;;   (tasks (concurrent (with-timeout 2 (shell "nc -l 43489"))
;;                      (shell "echo 1")
;;                      (sequential (shell "echo 2")
;;                                  (shell "sleep 4 && echo 3")))))

;; (tasks (concurrent (shell "date && sleep 1 && date")
;;                    (shell "for x in $(seq 1 3); do echo $x; sleep 1; done")))
;;(task-run (task-expand (isolate 'raw (shell "whoami"))))

(tasks (let ((w 5)
             (foo "echo hello"))
         (wait w)
         (shell foo)))
