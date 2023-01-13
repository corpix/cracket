#lang racket
(require racket/random
         corpix/bytes
         corpix/hex)
(provide current-multipart-form-random-source
         default-multipart-form-boundary-size
         current-multipart-form-boundary-size
         multipart-form-validate-boundary
         multipart-form-random-boundary

         (struct-out multipart-form)
         (struct-out multipart-form-field)
         multipart-form-content-type
         multipart-form-content-type-header
         multipart-form-content-disposition
         multipart-form-content-disposition-header
         multipart-form-write!
         multipart-form->bytes)

(define current-multipart-form-random-source
  (make-parameter crypto-random-bytes))

;;

(define-struct multipart-form
  (boundary fields)
  #:transparent)

(define-struct multipart-form-field
  (parms mime payload)
  #:transparent)

;;

(define default-multipart-form-boundary-size 30)
(define current-multipart-form-boundary-size
  (make-parameter default-multipart-form-boundary-size))

(define multipart-form-content-type-header
  (string->bytes/utf-8 "Content-Type"))

(define multipart-form-content-disposition-header
  (string->bytes/utf-8 "Content-Disposition"))

;;

(define boundary-escape
  (list->bytes
   (map ;; rfc2045 tspecials
    char->integer
    '(#\( #\) #\< #\>
      #\@ #\, #\; #\:
      #\" #\/ #\[ #\]
      #\? #\= #\space))))

;;

(define cr (char->integer #\return))
(define lf (char->integer #\newline))
(define crlf (bytes cr lf))
(define dash (char->integer #\-))
(define colon (char->integer #\:))
(define space (char->integer #\space))
(define colon-space (bytes colon space))
(define double-quote (char->integer #\"))
(define magic (bytes dash dash))

;;

(define (multipart-form-validate-boundary boundary)
  ;; modelled after https://golang.org/src/mime/multipart/writer.go#L45
  ;; rfc2046#section-5.1.1
  (define len (bytes-length boundary))
  (cond
    ((or (= len 0) (> len 70))
     (error "invalid boundary length"))
    (else (let loop ((n 0))
            (if (= n len)
                #t ;; reached boundary end
                (let ((chr (integer->char (bytes-ref boundary n))))
                  (cond
                    ((or (and (char-ci>=? chr #\A) (char-ci<=? chr #\Z))
                         (and (char>=?    chr #\0) (char<=?    chr #\9)))
                     (loop (+ n 1)))
                    ((memq chr '(#\' #\( #\)
                                 #\+ #\_ #\,
                                 #\- #\. #\/
                                 #\: #\= #\?))
                     (loop (+ n 1)))
                    ((and (char-whitespace? chr)
                          (not (= (+ n 1) len)))
                     (loop (+ n 1)))
                    (else (error (format "invalid boundary character at position ~a: ~s" n chr))))))))))

(define (multipart-form-random-boundary)
  (string->bytes/utf-8 (bytes->hex-string
                        ((current-multipart-form-random-source)
                         (current-multipart-form-boundary-size)))))

(define (multipart-form-content-type boundary)
  (let* ((escape? (bytes-contains-any boundary boundary-escape))
	 (boundary (if escape?
                       (bytes-append
                        (bytes double-quote) boundary
                        (bytes double-quote))
                       boundary)))
    (string-append "multipart/form-data; boundary=" (bytes->string/utf-8 boundary))))

;;(content-type (string->utf8 "test"))

(define (multipart-form-content-disposition type (rest '()))
  ;; TODO: better support for disposition-parm rfc2183#page-3
  (let loop ((acc type) (parms rest))
    (cond
      ((pair? parms)
       (let ((parm (car parms)))
         (loop
          (string-append acc "; "
                         (car parm) "=" (format "~s" (cdr parm)))
          (cdr parms))))
      (else acc))))

;;(content-disposition "form-data" '(("name" . "key1") ("foo" . "bar")))

(define (multipart-form-write! multipart-form (port (current-output-port)))
  (let* ((boundary  (multipart-form-boundary multipart-form))
	 (fields    (multipart-form-fields multipart-form))
	 (delimiter (bytes-append magic boundary)))
    (let loop ((rest fields))
      (cond
        ((pair? rest)
         (let*-values (((field)   (car rest))
                       ((name)    (car field))
                       ((value)   (cdr field))
                       ((parms mime payload)
                        (cond
                          ((bytes? value)
                           (values '(("filename" . "file"))
                                   "application/octet-stream"
                                   value))
                          ((port? value)
                           (values '(("filename" . "file"))
                                   "application/octet-stream"
                                   value))
                          ((number? value)
                           (values #f #f (number->string value)))
                          ((string? value)
                           (values #f #f value))
                          ((multipart-form-field? value)
                           (values (multipart-form-field-parms   value)
                                   (multipart-form-field-mime    value)
                                   (multipart-form-field-payload value)))
                          (else
                           (error (format "unexpected value: ~a" value))))))

           (write-bytes delimiter port)
           (write-bytes crlf port)
           ;;
           (write-bytes multipart-form-content-disposition-header port)
           (write-bytes colon-space port)
           (write-bytes (string->bytes/utf-8
                         (multipart-form-content-disposition
                          "form-data"
                          (append (list (cons "name" name))
                                  (or parms '()))))
                        port)
           (write-bytes crlf port)
           ;;
           (when mime
             (write-bytes multipart-form-content-type-header port)
             (write-bytes colon-space  port)
             (write-bytes (string->bytes/utf-8 mime) port)
             (write-bytes crlf port))
           ;;
           (write-bytes crlf port)
           (cond
             ((bytes? payload) (write-bytes payload port))
             ((port? payload) (copy-port payload port))
             ((string? payload) (write-bytes (string->bytes/utf-8 payload) port))
             (else (error "unexpected payload type: ~a" payload)))
           (write-bytes crlf port))
         (loop (cdr rest)))))
    (write-bytes delimiter port)
    (write-bytes magic port)
    (write-bytes crlf port)))

(define (multipart-form->bytes multipart-form)
  (let ((out (open-output-bytes)))
    (multipart-form-write! multipart-form out )
    (get-output-bytes out)))

;;

(module+ test
  (require rackunit)
  (test-case "multipart-form"
    (check-equal? (parameterize ((current-multipart-form-random-source (lambda (len) (make-bytes len))))
                    (let ((buf (open-output-bytes)))
                      (multipart-form-write!
                       (make-multipart-form
                        (multipart-form-random-boundary)
                        (list
                         (cons "foo" "foo val")
                         (cons "bar" "bar val")
                         (cons "test-file" (open-input-bytes (bytes 1 2 3)))
                         (cons "test-file-with-params" (multipart-form-field
                                                        '(("filename" . "untitled.png"))
                                                        "image/png"
                                                        (open-input-bytes (bytes 1 2 3))))))
                       buf)
                      (bytes->string/utf-8 (get-output-bytes buf))))
                  (string-join (list "--000000000000000000000000000000000000000000000000000000000000"
                                     "Content-Disposition: form-data; name=\"foo\""
                                     ""
                                     "foo val"
                                     "--000000000000000000000000000000000000000000000000000000000000"
                                     "Content-Disposition: form-data; name=\"bar\""
                                     ""
                                     "bar val"
                                     "--000000000000000000000000000000000000000000000000000000000000"
                                     "Content-Disposition: form-data; name=\"test-file\"; filename=\"file\""
                                     "Content-Type: application/octet-stream"
                                     ""
                                     "\u0001\u0002\u0003"
                                     "--000000000000000000000000000000000000000000000000000000000000"
                                     "Content-Disposition: form-data; name=\"test-file-with-params\"; filename=\"untitled.png\""
                                     "Content-Type: image/png"
                                     ""
                                     "\u0001\u0002\u0003"
                                     "--000000000000000000000000000000000000000000000000000000000000--"
                                     "")
                               "\r\n"))))
