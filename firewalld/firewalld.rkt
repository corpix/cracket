#lang racket
(require racket/os
         web-server/dispatch
         web-server/http
         web-server/servlet-dispatch
         web-server/web-server
         net/ip
         corpix/bnf
         corpix/time
         corpix/hex
         corpix/prometheus)
(module+ test
  (require rackunit))

(define current-http-address (make-parameter "127.0.0.1"))
(define current-http-port (make-parameter 5634))
(define current-by-ip-limit (make-parameter 20))
(define current-ttl (make-parameter (hour-duration 2)))
(define current-ttl-timer (make-parameter 60))
(define current-firewall (make-parameter 'iptables))

(define-bnf firewall-parse-line
  ((space " ")
   (colon ":")
   (dot ".")
   (eq "=")
   (spaces (+ space))
   (alpha-lower (or "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "a" "s" "d" "f" "g" "h" "j" "k" "l" "z" "x" "c" "v" "b" "n" "m"))
   (alpha-upper (or "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P" "A" "S" "D" "F" "G" "H" "J" "K" "L" "Z" "X" "C" "V" "B" "N" "M"))
   (alpha (or alpha-lower alpha-upper))
   (alphas (+ alpha))
   (number (or "1" "2" "3" "4" "5" "6" "7" "8" "9" "0"))
   (numbers (+ number))
   (month (or "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
   (day numbers)
   (time (and numbers colon numbers colon numbers))
   (date (and month space day space time))
   (hostname (or alphas numbers))
   (key (+ alpha-upper))
   (value (* (or alpha number colon dot)))
   (key-value (and key (* eq) value))
   (line (and date
              space
              hostname space
              "kernel" colon space
              (+ (or alpha space)) colon space
              (+ (and key-value (* space)))
              )))
  line)

(define-bnf getpcaps-parse-line
  ((space " ")
   (colon ":")
   (comma ",")
   (eq "=")
   (spaces (+ space))
   (alpha-lower (or "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "a" "s" "d" "f" "g" "h" "j" "k" "l" "z" "x" "c" "v" "b" "n" "m"))
   (alpha-upper (or "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P" "A" "S" "D" "F" "G" "H" "J" "K" "L" "Z" "X" "C" "V" "B" "N" "M"))
   (alpha (or alpha-lower alpha-upper))
   (number (or "1" "2" "3" "4" "5" "6" "7" "8" "9" "0"))
   (flags (+ (or "=" "+" "e" "i" "p")))
   (capability (+ (or alpha number "_")))
   (capabilities (* (and (or capability flags) (* flags) (* comma))))
   (prefix (and (+ number) colon (* space)))
   (line (and prefix capabilities)))
  line)

;;

(define current-logger (make-parameter
                        (lambda (msg)
                          (displayln msg)
                          (flush-output))))
(define (log message)
  ((current-logger) message))

;;

(define (command executable . arguments)
  (match-let (((list out in pid err control)
               (apply process* (find-executable-path executable)
                      arguments)))
    (close-output-port in)
    (let* ((io (for/list ((evt (for/list ((port (list out err)))
                                 (thread (lambda ()
                                           (for ((line (in-lines port)))
                                             (when (> (string-length line) 0)
                                               (log line))))))))
                 evt)))
      (control 'wait)
      (dynamic-wind void
                    (thunk (let ((code (control 'exit-code)))
                             (when (not (= code 0))
                               (error (format "shell command '~a' exited with ~a code, err: ~a"
                                              (append (list executable) arguments) code
                                              (string-trim (port->string err) "\n"
                                                           #:left? #t
                                                           #:right? #t))))))
                    (thunk (for ((port (in-list io)))
                             (sync port))
                           (close-input-port out)
                           (close-input-port err))))))
;;

(define (make-firewall-iptables)
  (lambda (fields proto in ip)
    (command (if (= (ip-address-version (make-ip-address (hash-ref fields "SRC"))) 4)
                 "iptables"
                 "ip6tables")
             "-I" "INPUT"
             "-p" proto
             "-i" in
             "--src" ip
             "-j" "DROP")))

(define (make-firewall-dummy)
  (lambda (fields proto in ip)
    (log (format "dummy firewall fields ~a, proto ~a, in ~a, ip ~a"
                 fields proto in ip))))

(define current-firewalls
  (make-parameter (make-hasheq `((iptables . ,(make-firewall-iptables))
                                 (dummy . ,(make-firewall-dummy))))))

;;

(define-struct firewall-refused
  (date hostname fields)
  #:transparent)

(define (firewall-fold-line ast)
  (make-firewall-refused
   (string->datetime (bytes->string/utf-8
                      (bnf-node-value (vector-ref (bnf-node-collect ast 'date) 0)))
                     "~b ~d ~H:~M:~S")
   (bytes->string/utf-8 (bnf-node-value (vector-ref (bnf-node-collect ast 'hostname) 0)))
   (for/hash ((key (in-vector (bnf-node-collect ast 'key)))
              (value (in-vector (bnf-node-collect ast 'value))))
     (let ((k (bytes->string/utf-8 (bnf-node-value key)))
           (v (bytes->string/utf-8 (bnf-node-value value))))
       (values k
               (cond
                 ((or (equal? k "DST") (equal? k "SRC"))
                  ;; (make-ip-address v)
                  v)
                 ((or (equal? k "DPT") (equal? k "SPT") (equal? k "LEN") (equal? k "ID") (equal? k "TTL"))
                  (string->number v))
                 ((or (equal? k "IN") (equal? k "OUT") (equal? k "PROTO"))
                  (if (> (string-length v) 0) (string->symbol v) #f))
                 ((or (equal? k "PREC") (equal? k "TOS"))
                  (hex-string->integer (string-trim v "0x" #:left? #t #:right? #f)))
                 (else (if (> (string-length v) 0) v #f))))))))

;;

(define (check-in-firewall-lines p mode)
  (unless (input-port? p) (raise-argument-error 'in-firewall-lines "input-port?" p))
  (unless (memq mode '(linefeed return return-linefeed any any-one))
    (raise-argument-error
     'in-firewall-lines
     "(or/c 'linefeed 'return 'return-linefeed 'any 'any-one)"
     mode)))

(define-sequence-syntax in-firewall-lines
  (lambda () #'in-firewall-lines)
  (lambda (stx)
    (syntax-case stx ()
      (((id) (_))   #'((id) (in-firewall-lines (current-input-port) 'any)))
      (((id) (_ p)) #'((id) (in-firewall-lines p 'any)))
      (((id) (_ p mode))
       #'((id) (in-producer
                (let ((p* p) (mode* mode))
                  (check-in-firewall-lines p* mode*)
                  (lambda ()
                    (let next ()
                      (let ((line (read-line p* mode*)))
                        (with-handlers ((exn? (lambda (exn)
                                                (log (format
                                                      "got an exception ~s while handling line ~s"
                                                      exn line))
                                                (next))))
                          (if (eof-object? line)
                              line
                              (firewall-fold-line (firewall-parse-line line))))))))
                eof)))
      (_ #f))))

;;

(define (worker port)
  (let* ((by-ip (make-prometheus-metric-counter 'firewall_refused_ip))
         (by-ip-label-names (set "DPT" "DST" "IN" "MAC" "OUT" "PROTO" "SRC" "TOS"))
         (by-ip-limit (current-by-ip-limit))
         (banned (make-prometheus-metric-counter 'firewall_refused_ip_banned))
         (ttl (make-hasheq))
         (expire (vector))
         (sem (make-semaphore 1))
         (done (make-semaphore 0))
         (cleaner (thread (thunk ;; stat record expiration worker
                           (let loop ((time (current-time)))
                             (call-with-semaphore
                              sem
                              (thunk
                               (for ((to-expire (in-vector expire))
                                     (n (in-naturals)))
                                 #:break (and (time>? (car to-expire) time)
                                              (begin0 #t (set! expire (vector-drop expire n))))
                                 (log (format "expiring stat record ~a, ttl ~a > ~a" (cdr to-expire) time (car to-expire)))
                                 (prometheus-erase! by-ip #:labels (cdr to-expire))
                                 (hash-remove! ttl (cdr to-expire)))))
                             (sleep (current-ttl-timer))
                             (or (semaphore-try-wait? done)
                                 (loop (current-time))))))))
    (prometheus-register! by-ip)
    (prometheus-register! banned)
    (for ((refused (in-firewall-lines port)))
      (let* ((fields (firewall-refused-fields refused))
             (by-ip-labels (filter (lambda (field)
                                     (and (set-member? by-ip-label-names (car field))
                                          (cdr field)))
                                   (hash->list fields)))
             (by-ip-value (let ((value (prometheus-increment! by-ip #:labels by-ip-labels)))
                            (call-with-semaphore
                             sem
                             (thunk
                              (let ((time (current-time)))
                                (hash-set! ttl by-ip-labels time)
                                (set! expire (vector-append expire (vector (cons time by-ip-labels)))))))
                            value)))
        (when (and (> by-ip-value by-ip-limit)
                   (not (prometheus-metric-value banned #f #:labels by-ip-labels)))
          (let ((proto (symbol->string (hash-ref fields "PROTO")))
                (in (symbol->string (hash-ref fields "IN")))
                (ip (hash-ref fields "SRC")))
            (prometheus-increment! banned #:labels by-ip-labels)
            (log (format "about to ban incoming protocol ~s packets from ip ~s for interface ~s" proto ip in))
            ((hash-ref (current-firewalls) (current-firewall))
             fields proto in ip)))))
    (log "worker is finished internal job processing loop")
    (semaphore-post done)
    (void (thread-wait cleaner))))

;;

(define (pump)
  (match-define (list out in _ err control)
    (process* (find-executable-path "journalctl") "-k" "--follow"))
  (close-output-port in)
  (worker out)
  (control 'wait)
  (close-input-port out)
  (close-input-port err))

(define (http)
  (define-values (app _)
    (dispatch-rules
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
  (define stop (start (current-http-address)
                      (current-http-port)))
  (with-handlers ((exn:break? (thunk*
                               (displayln "stopping web server")
                               (stop))))
    (sync never-evt)))

(define (main)
  (for ((job (in-list (list
                       (thread pump)
                       (thread http)))))
    (sync job)))

;;

(define (preflight)
  (match-define (list out in _ err control)
    (process* (find-executable-path "getpcaps") (number->string (getpid))))
  (close-output-port in)
  (for ((line (in-lines out)))
    (and
     (> (string-length line) 0)
     (unless (vector-member #"cap_net_admin"
                            (vector-map bnf-node-value
                                        (bnf-node-collect
                                         (getpcaps-parse-line line)
                                         'capability)))
       (error "current process can not modify firewall because cap_net_admin is not set, use init system to fix this"))))
  (control 'wait)
  (close-input-port out)
  (close-input-port err))

;;

(module+ main
  (preflight)
  (main))

;;

(module+ test
  (test-case "worker"
    (check-equal?
     (parameterize ((current-firewall 'dummy)
                    (current-by-ip-limit 1)
                    (current-ttl-timer 1)
                    (current-logger void))
       (let ((log-lines (string-join (list
                                      "Jan 27 13:04:22 test kernel: refused packet: IN=eth0 OUT= MAC=00:00:00:00:00:00:00:00:00:00:00:00:00:00 SRC=40.99.214.34 DST=127.0.0.1 LEN=92 TOS=0x00 PREC=0x00 TTL=243 ID=39398 DF PROTO=TCP SPT=443 DPT=59156 WINDOW=16382 RES=0x00 ACK PSH URGP=0"
                                      "Jan 27 13:04:22 test kernel: refused packet: IN=eth0 OUT= MAC=00:00:00:00:00:00:00:00:00:00:00:00:00:00 SRC=40.99.214.34 DST=127.0.0.1 LEN=129 TOS=0x00 PREC=0x00 TTL=243 ID=39399 DF PROTO=TCP SPT=443 DPT=59156 WINDOW=16382 RES=0x00 ACK PSH URGP=0"
                                      "Jan 27 13:04:22 test kernel: refused packet: IN=eth0 OUT= MAC=00:00:00:00:00:00:00:00:00:00:00:00:00:00 SRC=40.99.214.34 DST=127.0.0.1 LEN=984 TOS=0x00 PREC=0x00 TTL=243 ID=39400 DF PROTO=TCP SPT=443 DPT=59156 WINDOW=16382 RES=0x00 ACK PSH URGP=0"
                                      "Jan 27 13:04:22 test kernel: refused packet: IN=eth0 OUT= MAC=00:00:00:00:00:00:00:00:00:00:00:00:00:00 SRC=40.99.214.34 DST=127.0.0.1 LEN=392 TOS=0x00 PREC=0x00 TTL=243 ID=39401 DF PROTO=TCP SPT=443 DPT=59156 WINDOW=16382 RES=0x00 ACK PSH URGP=0"
                                      "Jan 27 13:04:22 test kernel: refused packet: IN=eth0 OUT= MAC=00:00:00:00:00:00:00:00:00:00:00:00:00:00 SRC=40.99.214.34 DST=127.0.0.1 LEN=410 TOS=0x00 PREC=0x00 TTL=243 ID=39402 DF PROTO=TCP SPT=443 DPT=59156 WINDOW=16382 RES=0x00 ACK PSH URGP=0")
                                     "\n")))
         (worker (open-input-string log-lines))
         (prometheus-metrics-format)))
     "# TYPE firewall_refused_ip_banned counter\nfirewall_refused_ip_banned{SRC=\"40.99.214.34\",IN=\"eth0\",PROTO=\"TCP\",DPT=\"59156\",TOS=\"0\",DST=\"127.0.0.1\",MAC=\"00:00:00:00:00:00:00:00:00:00:00:00:00:00\"} 1\n# TYPE firewall_refused_ip counter\nfirewall_refused_ip{SRC=\"40.99.214.34\",IN=\"eth0\",PROTO=\"TCP\",DPT=\"59156\",TOS=\"0\",DST=\"127.0.0.1\",MAC=\"00:00:00:00:00:00:00:00:00:00:00:00:00:00\"} 5\n"))
  (test-case "getpcaps-parse-line"
    (check-equal?
     (vector-map bnf-node-value
                 (bnf-node-collect
                  (getpcaps-parse-line "402438: =")
                  'capability))
     (vector))
    (check-equal?
     (vector-map bnf-node-value
                 (bnf-node-collect
                  (getpcaps-parse-line "402438: cap_net_admin")
                  'capability))
     (vector #"cap_net_admin"))
    (check-true
     (and (vector-member #"cap_net_admin"
                         (vector-map bnf-node-value
                                     (bnf-node-collect
                                      (getpcaps-parse-line "402438: cap_net_admin")
                                      'capability)))
          #t))))
