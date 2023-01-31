#lang racket
;; FIXME: how the fuck should I disable printing stack traces to user?
(require web-server/dispatch
         web-server/http
         web-server/servlet-dispatch
         web-server/web-server
         corpix/prometheus
         corpix/json)

(define (event-type req)
  (let* ((headers (request-headers/raw req))
         (found (for/fold ((type #f))
                          ((name (list #"X-GitHub-Event"
                                       #"X-Gitea-Event"
                                       #"X-Gogs-Event")))
                  #:break (not (eq? type #f))
                  (headers-assq* name headers))))
    (and found (header-value found))))

;; ssh key to pull the code
;; ssh key should be added to the target repository
;; webhook to receive updates
;; glue to run tasks for the repository with (predefined) isolation

(define (dispatch-event req)
  (let* ((type (event-type req))
         (payload (and type (bytes->json (request-post-data/raw req)))))
    (when payload
      (displayln (list
                  (hash-ref payload "ref" #f)
                  (let ((commit (hash-ref payload "head_commit" #f)))
                    (and commit (hash-ref commit "id" #f)))
                  (let ((repository (hash-ref payload "repository" #f)))
                    (and repository (hash-ref repository "ssh_url")))
                  (let ((pusher (hash-ref payload "pusher" #f)))
                    (and pusher (list (hash-ref pusher "login" #f)
                                      (hash-ref pusher "email" #f))))
                  (let ((sender (hash-ref payload "sender" #f)))
                    (and sender (list (hash-ref sender "login" #f)
                                      (hash-ref sender "email" #f))))))))
  (response/output void))

(define-values (app _)
  (dispatch-rules
   (("") #:method "post" dispatch-event)
   (("metrics") (make-prometheus-http-handler))
   (else (thunk* (response/output
                  #:code 404
                  (lambda (out) (displayln "not found" out)))))))

(define (start host port)
  (displayln (format "running web server on ~a:~a" host port))
  (let ((jobs (list (prometheus-start-runtime-collector)
                    (serve #:dispatch (dispatch/servlet app)
                           #:listen-ip host
                           #:port port))))
    (thunk (for ((stop jobs)) (stop)))))

(module+ main
  (define stop (start "127.0.0.1" 8080))
  (with-handlers ((exn:break? (thunk*
                               (displayln "stopping web server")
                               (stop))))
    (sync never-evt)))
