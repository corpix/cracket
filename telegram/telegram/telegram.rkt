#lang racket
(require corpix/http
         corpix/json
         (for-syntax racket/syntax))
(provide current-telegram-token
         current-telegram-url
         current-telegram-timeout
         current-telegram-poll-timeout
         current-telegram-log

         define-telegram
         telegram-get-me
         telegram-get-updates
         telegram-send-message
         telegram-send-photo
         telegram-answer-inline-query
         telegram-loop)


;;

(define current-telegram-token (make-parameter ""))
(define current-telegram-url (make-parameter "https://api.telegram.org"))
(define current-telegram-timeout (make-parameter 5))
(define current-telegram-poll-timeout (make-parameter 120))
(define current-telegram-log (make-parameter displayln))

;;

(define (uri method)
  (string-append (current-telegram-url)
		 "/bot" (current-telegram-token)
		 "/" method))

;;

(define (encode-query payload)
  (for/list ((kv payload))
    (cons (car kv) (format "~a" (cdr kv)))))

(define (encode-json payload)
  (json->string (make-hash payload)))

(define (decode-json response)
  (let ((payload (read-json (http-response-body-reader response))))
    (unless (hash-ref payload "ok")
      (error "server responded with an error: "
             (hash-ref payload "error_code")
             (hash-ref payload "description" "")))
    (hash-ref payload "result")))

;;


;; FIXME: rewrite using syntax-case
(begin-for-syntax
  (define (payload-syntax kwargs)
    (define pairs '())
    (for ((arg (syntax->list kwargs)))
      (let ((argp (syntax-e arg)))
	(cond
          ((symbol? argp) ;; required argument
           (with-syntax ((key argp)
                         (value arg))
             (set! pairs
               (cons (syntax (cons 'key value))
                     pairs))))
          ((pair? argp) ;; optional argument
           (let* ((sym (car argp))
                  (name (syntax->datum sym)))
             (with-syntax ((key name)
                           (value sym))
               (set! pairs
                 (cons (syntax (cons 'key value))
                       pairs))))))))
    (cons 'list pairs)))

;;(kwargs->alist-stx (syntax (foo: (bar 0) baz: (qux 1) ducks: oops)))
;; => (list (cons qux qux) (cons bar bar))

(define-syntax (define-telegram stx)
  (syntax-case stx ()
    ((_ (name http-method method-name)
	(kind arguments ...)
	(encode decode))
     (let ((request-kind (syntax->datum #'kind)))
       (with-syntax* ((headers (case request-kind
				 ((query multipart) (syntax '()))
				 ((json)            (syntax '(("Content-Type" . "application/json"))))))
		      (payload (payload-syntax #'(arguments ...)))
		      (body-kw (cond
                                 ((eq? request-kind 'query) (syntax #:query))
                                 ((eq? request-kind 'json) (syntax #:body))
                                 ((eq? request-kind 'multipart) (syntax #:multipart))
                                 (else (error (format "unsupported request kind: ~a" request-kind))))))
	 (syntax/loc stx
           (define (name arguments ...)
             (with-request (res (http (uri method-name)
                                      body-kw (if encode (encode payload) payload)
                                      #:method http-method
                                      #:headers headers))
               (decode res)))))))))

(define-telegram (telegram-get-me 'GET "getMe")
  (query)
  (encode-query decode-json))

(define-telegram (telegram-get-updates 'GET "getUpdates")
  (query #:limit (limit 100)
	 #:offset (offset 0)
	 #:timeout (timeout (current-telegram-poll-timeout)))
  (encode-query decode-json))

(define-telegram (telegram-send-message 'POST "sendMessage")
  (json #:chat-id chat_id
	#:text text)
  (encode-json decode-json))

(define-telegram (telegram-send-photo 'POST "sendPhoto")
  (multipart #:chat-id chat_id
	     #:photo photo
	     #:caption (caption ""))
  (#f decode-json))

(define-telegram (telegram-answer-inline-query 'POST "answerInlineQuery")
  (json #:query-id inline_query_id
	#:answers results)
  (encode-json decode-json))

;;

(define-syntax telegram-loop
  (syntax-rules ()
    ((_ sym body0 body ...)
     (let ((thunk (lambda (sym) body0 body ...))
	   (offset 0))
       (let loop ((updates (telegram-get-updates)))
	 (for ((update updates))
	   (unless (hash? update)
	     (error "expected update to be a hash-table, got:" update))
	   (set! offset (hash-ref update "update_id"))
	   (current-telegram-log (json->string update))
	   (thunk update))
	 (loop (telegram-get-updates #:offset (+ 1 offset))))))))

(module+ test
  (require rackunit)
  (test-case "encode-query"
    (check-equal? '((1 . "1") (2 . "a"))
                  (encode-query (make-hasheq '((1 . 1) (2 . "a"))))))
  (test-case "encode-json"
    ;; NOTE: hashes are unordered, so decoding json to make tests stable
    (check-equal? (string->json "{\"a\":1,\"b\":2}")
                  (string->json (encode-json '((a . 1) (b . 2)))))))
