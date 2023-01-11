#lang racket
(require (for-syntax racket/syntax)
         corpix/http
         corpix/json)
(provide current-token
         current-url
         current-timeout
         current-poll-timeout
         current-log

         get-me
         get-updates
         send-message
         send-photo
         answer-inline-query

         dispatch
         for/telegram)


;;

(define current-token (make-parameter ""))
(define current-url (make-parameter "https://api.telegram.org"))
(define current-timeout (make-parameter 5))
(define current-poll-timeout (make-parameter 120))
(define current-log (make-parameter displayln))

;;

(define (uri method)
  (string-append (current-url)
		 "/bot" (current-token)
		 "/" method))

;;

(define (encode-query payload)
  (for/list (((k v) payload))
    (cons k (format "~a" v))))

(define (encode-json payload)
  (json->string (make-hasheq payload)))

(define (decode-json response)
  (let ((payload (read-json (response-body-reader response))))
    (unless (hash-ref payload 'ok)
      (error "server responded with an error: "
             (hash-ref payload 'error_code)
             (hash-ref payload 'description "")))
    (hash-ref payload 'result)))

;;

(begin-for-syntax
  (define (payload-syntax kwargs)
    (define pairs '())
    (for ((arg (syntax->list kwargs)))
      (let ((argp (syntax-e arg)))
	(cond
          ((symbol? argp) ;; required argument
           (with-syntax ((key (symbol->string argp))
                         (value arg))
             (set! pairs
               (cons (syntax (cons key value))
                     pairs))))
          ((pair? argp) ;; optional argument
           (let* ((sym (car argp))
                  (name (syntax->datum sym)))
             (with-syntax ((key (symbol->string name))
                           (value sym))
               (set! pairs
                 (cons (syntax (cons key value))
                       pairs))))))))
    (cons 'list pairs)))
;;(kwargs->alist-stx (syntax (foo: (bar 0) baz: (qux 1) ducks: oops)))
;; => (list (cons qux qux) (cons bar bar))

(define-syntax (define-botapi-method stx)
  (syntax-case stx ()
    ((_ (name http-method method-name)
	(kind arguments ...)
	(encode decode))
     (let ((request-kind (syntax->datum #'kind)))
       (with-syntax* ((headers (case request-kind
				 ((query multipart) (syntax #f))
				 ((json)            (syntax '(("Content-Type" . "application/json"))))))
		      (payload (payload-syntax #'(arguments ...)))
		      (request-key (cond
                                     ((eq? request-kind 'query) 'query)
                                     ((eq? request-kind 'json) 'body)
                                     ((eq? request-kind 'multipart) 'multipart)
                                     (else (error (format "unsupported request kind: ~a" request-kind))))))
	 (syntax (define (name arguments ...)
		   (displayln method-name)
		   (with-request (req (http-request http-method (uri method-name)
						    request-key (if encode (encode payload) payload)
						    headers: headers))
                     (decode req)))))))))

(define-botapi-method
  (get-me 'GET "getMe")
  (query)
  (encode-query decode-json))

(define-botapi-method
  (get-updates 'GET "getUpdates")
  (query #:limit (limit 100)
	 #:offset (offset 0)
	 #:timeout (timeout (current-poll-timeout)))
  (encode-query decode-json))

(define-botapi-method
  (send-message 'POST "sendMessage")
  (json #:chat-id chat_id
	#:text text)
  (encode-json decode-json))

(define-botapi-method
  (send-photo 'POST "sendPhoto")
  (multipart #:chat-id chat_id
	     #:photo photo
	     #:caption (caption ""))
  (#f decode-json))

(define-botapi-method
  (answer-inline-query 'POST "answerInlineQuery")
  (json #:query-id inline_query_id
	#:answers results)
  (encode-json decode-json))

;;

(define-syntax dispatch
  (syntax-rules (<>)
    ((dispatch <> case0 cases ...)
     (lambda (t) (dispatch t case0 cases ...)))
    ((dispatch table (key action) ...)
     (let ((t table))
       (and (hash-table? t)
	    (let ((results
		   (cond*
		    ((hash-ref t key #f) => action) ...)))
	      (if (pair? results)
                  results #f)))))))

(define-syntax for/telegram
  (syntax-rules ()
    ((for/telegram sym body0 body ...)
     (let ((thunk (lambda (sym) body0 body ...))
	   (offset 0))
       (let loop ((updates (get-updates)))
	 (for ((update updates))
	   (unless (hash-table? update)
	     (error "expected update to be a hash-table, got:" update))
	   (set! offset (hash-ref update 'update_id))
	   (current-log (json-object->string update))
	   (thunk update))
	 (loop (get-updates offset: (+ 1 offset))))))))

(module+ test
  (require rackunit)
  (test-case "encode-query"
    (equal? '((1 . "1") (2 . "a"))
            (encode-query (make-hasheq '((1 . 1) (2 . "a"))))))
  (test-case "encode-json"
    (equal?
     "{\"a\":1,\"b\":2}"
     (encode-json '((a . 1) (b . 2))))))
