#lang racket
(require "task.rkt")

(void (thread (thunk
               (let loop ()
                 (define log-vector (sync task-logger-receiver))
                 (displayln
                  (format "[~a] ~a"
                          (vector-ref log-vector 0)
                          (vector-ref log-vector 1))
                  (current-error-port))
                 (loop)))))

(tasks (let ((port-number 6688)
             (custodian (make-custodian (current-custodian))))
         (parameterize ((current-custodian custodian))
           (concurrent (command (format "sleep 5 && nc -l ~a" port-number))
                       (sequential (->> (command "echo ready")
                                        (port port-number)
                                        (timeout 15))
                                   (displayln 'shutdown)
                                   (custodian-shutdown-all custodian))))))
