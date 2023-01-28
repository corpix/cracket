#lang racket
(require net/ip
         corpix/bnf
         corpix/time
         corpix/hex
         corpix/prometheus)
(module+ test
  (require rackunit))

(define current-logger (make-parameter displayln))

(define (command executable . arguments)
  (match-let (((list out in pid err control)
               (apply process* (find-executable-path executable)
                      arguments)))
    (close-output-port in)
    (let* ((logger (current-logger))
           (io (for/list ((evt (for/list ((port (list out err)))
                                 (thread (lambda ()
                                           (for ((line (in-lines port)))
                                             (when (> (string-length line) 0)
                                               (logger line))))))))
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
                  (lambda () (let ((line (read-line p* mode*)))
                               (if (eof-object? line)
                                   line
                                   (firewall-fold-line (firewall-parse-line line))))))
                eof)))
      (_ #f))))

;;

(define (worker port)
  (let ((by-ip (make-prometheus-metric-counter 'firewall_refused_ip))
        (by-ip-label-names (set "DPT" "DST" "IN" "MAC" "OUT" "PROTO" "SPT" "SRC" "TOS"))
        (by-ip-limit 20)
        (by-ip-port (make-prometheus-metric-counter 'firewall_refused_ip_port))
        (by-ip-port-label-names (set "DST" "IN" "MAC" "OUT" "PROTO" "SRC" "TOS"))
        (banned (make-prometheus-metric-counter 'firewall_refused_ip_banned)))
    (prometheus-register! by-ip)
    (prometheus-register! by-ip-port)
    (prometheus-register! banned)
    (for ((refused (in-firewall-lines out)))
      (let* ((fields (firewall-refused-fields refused))
             (by-ip-labels (filter (lambda (field) (set-member? by-ip-label-names (car field)))
                                   (hash->list fields)))
             (by-ip-port-labels (filter (lambda (field) (set-member? by-ip-port-label-names (car field)))
                                        (hash->list fields)))
             (by-ip-value (begin0 (prometheus-increment! by-ip #:labels by-ip-labels)
                            (prometheus-increment! by-ip-port #:labels by-ip-port-labels))))
        (when (> by-ip-value by-ip-limit)
          (command (if (= (ip-address-version (make-ip-address (hash-ref fields "SRC"))) 4)
                       "iptables"
                       "ip6tables")
                   "-I" "INPUT"
                   "-p" (symbol->string (hash-ref fields "PROTO"))
                   "-j" "DROP")
          (prometheus-increment! banned #:labels by-ip-labels))))))

;;

;; (match-define (list out in _ err control)
;;   (process* "./stub"))
;; (close-output-port in)
;; (close-input-port err)
;; (worker out)
;; (displayln (prometheus-metrics-format))
;; (close-input-port out)
