#lang racket
(require racket/serialize
         srfi/29
         srfi/8/receive
         srfi/optional)
(provide time-iso8601
         time-iso8601-nano

         ;;

         nanosecond
         microsecond
         millisecond
         second
         minute
         hour
         day
         week

         ;;

         nanosecond-duration
         microsecond-duration
         millisecond-duration
         second-duration
         minute-duration
         hour-duration
         day-duration
         week-duration
         duration-units
         string->duration
         duration->string

         ;;

         unix-microseconds
         unix-milliseconds
         unix-seconds
         time->time-utc
         time->unix-nano
         time->datetime
         datetime->time
         time->string
         string->time

         time-tai
         time-utc
         time-monotonic
         time-thread
         time-process
         time-duration
         time-gc

         current-date
         current-julian-day
         current-modified-julian-day
         current-time
         time-resolution

         make-time
         time?
         time-type
         time-nanosecond

         time-second
         set-time-type!
         set-time-nanosecond!
         set-time-second!
         copy-time

         time<=?
         time<?
         time=?
         time>=?
         time>?

         time-difference
         time-difference!
         add-duration
         add-duration!
         subtract-duration
         subtract-duration!

         make-datetime
         datetime?
         datetime*?
         datetime-nanosecond
         datetime-second
         datetime-minute
         datetime-hour
         datetime-day
         datetime-month
         datetime-year
         datetime-zone-offset

         datetime-year-day
         datetime-week-day
         date-week-number

         datetime->julian-day
         datetime->modified-julian-day
         datetime->time-monotonic
         datetime->time-tai
         datetime->time-utc

         julian-day->datetime
         julian-day->time-monotonic
         julian-day->time-tai
         julian-day->time-utc

         modified-julian-day->datetime
         modified-julian-day->time-monotonic
         modified-julian-day->time-tai
         modified-julian-day->time-utc

         time-monotonic->datetime
         time-monotonic->julian-day
         time-monotonic->modified-julian-day
         time-monotonic->time-tai
         time-monotonic->time-tai!
         time-monotonic->time-utc
         time-monotonic->time-utc!

         time-tai->datetime
         time-tai->julian-day
         time-tai->modified-julian-day
         time-tai->time-monotonic
         time-tai->time-monotonic!
         time-tai->time-utc
         time-tai->time-utc!
         time-utc->datetime
         time-utc->julian-day
         time-utc->modified-julian-day
         time-utc->time-monotonic
         time-utc->time-monotonic!
         time-utc->time-tai
         time-utc->time-tai!

         datetime->string
         string->datetime)
(module+ test
  (require rackunit))

;;

(define localized? #f)
(define localized-message
  (lambda (message-name)
    (unless localized?
      (re-read-locale)
      (or (load-bundle! (list* 'srfi-19
                               (current-language)
                               (current-country)
                               (current-locale-details)))
          (load-bundle! (list 'srfi-19
                              (current-language)
                              (current-country)))
          (load-bundle! (list 'srfi-19 (current-language)))
          (load-bundle! (list 'srfi-19)))
      (set! localized? #t))

    (localized-template 'srfi-19 message-name)))

(define time-tai 'time-tai)
(define time-utc 'time-utc)
(define time-monotonic 'time-monotonic)
(define time-thread 'time-thread)
(define time-process 'time-process)
(define time-duration 'time-duration)
(define time-gc 'time-gc)

(define locale-number-separator 'separator)
(define locale-abbr-weekday-vector (vector 'sun 'mon 'tue 'wed 'thu 'fri 'sat))
(define locale-long-weekday-vector (vector 'sunday 'monday 'tuesday 'wednesday
                                           'thursday 'friday 'saturday))
(define locale-abbr-month-vector (vector 'jan 'feb 'mar
                                         'apr 'may 'jun 'jul
                                         'aug 'sep 'oct 'nov
                                         'dec))
(define locale-long-month-vector (vector 'january 'february
                                         'march 'april 'may
                                         'june 'july 'august
                                         'september 'october
                                         'november 'december))

(define locale-pm 'pm)
(define locale-am 'am)
(define locale-date-time-format 'date-time)
(define locale-short-date-format 'date)
(define locale-time-format 'time)
(define iso-8601-date-time-format 'iso8601)
(define nano (expt 10 9))
(define sid  86400) ;; seconds in a day
(define sihd (/ sid 2)) ;; seconds in a half day
(define tai-epoch-in-jd 4881175/2) ;; julian day number for 'the epoch'
(define time-error-types
  '(invalid-clock-type
    unsupported-clock-type
    incompatible-time-types
    not-duration
    dates-are-immutable
    bad-date-format-string
    bad-date-template-string
    invalid-month-specification))

(define (time-error caller type value)
  (if (member type time-error-types)
      (if value
          (error caller "time-error: type ~S: ~S" type value)
          (error caller "time-error: type ~S" type))
      (error caller "time-error: unsupported error type ~S" type)))

(define (read-tai-utc-data filename)
  (define (convert-jd jd)
    (* (- (inexact->exact jd) tai-epoch-in-jd) sid))
  (define (convert-sec sec)
    (inexact->exact sec))
  (let ((port (open-input-file filename))
        (table '()))
    (let loop ((line (read-line port)))
      (unless (eq? line eof)
        (let* ((data (read (open-input-string (string-append "(" line ")"))))
               (year (car data))
               (jd   (cadddr (cdr data)))
               (secs (cadddr (cdddr data))))
          (when (>= year 1972)
            (set! table (cons (cons (convert-jd jd) (convert-sec secs)) table)))
          (loop (read-line port)))))
    table))

(define leap-second-table
  '((1435708800 . 36) ; 2015-07-01
    (1341100800 . 35) ; 2012-07-01
    (1230768000 . 34) ; 2009-01-01
    (1136073600 . 33) ; 2006-01-01
    (915148800  . 32) ; 1999-01-01
    (867715200  . 31) ; 1997-07-01
    (820454400  . 30) ; 1996-01-01
    (773020800  . 29) ; 1994-07-01
    (741484800  . 28) ; 1993-07-01
    (709948800  . 27) ; 1992-07-01
    (662688000  . 26) ; 1991-01-01
    (631152000  . 25) ; 1990-01-01
    (567993600  . 24) ; 1988-01-01
    (489024000  . 23) ; 1985-07-01
    (425865600  . 22) ; 1983-07-01
    (394329600  . 21) ; 1982-07-01
    (362793600  . 20) ; 1981-07-01
    (315532800  . 19) ; 1980-01-01
    (283996800  . 18) ; 1979-01-01
    (252460800  . 17) ; 1978-01-01
    (220924800  . 16) ; 1977-01-01
    (189302400  . 15) ; 1976-01-01
    (157766400  . 14) ; 1975-01-01
    (126230400  . 13) ; 1974-01-01
    (94694400   . 12) ; 1973-01-01
    (78796800   . 11) ; 1972-07-01
    (63072000   . 10) ; 1972-01-01
    ))

(define (read-leap-second-table filename)
  (set! leap-second-table (read-tai-utc-data filename))
  (values))


(define (leap-second-delta utc-seconds)
  (letrec ((lsd (lambda (table)
                  (cond ((>= utc-seconds (caar table))
                         (cdar table))
                        (else (lsd (cdr table)))))))
    (if (< utc-seconds  (* (- 1972 1970) 365 sid)) 0
        (lsd  leap-second-table))))

(define (leap-second-neg-delta tai-seconds)
  (letrec ((lsd (lambda (table)
                  (cond ((null? table) 0)
                        ((<= (cdar table) (- tai-seconds (caar table)))
                         (cdar table))
                        (else (lsd (cdr table)))))))
    (if (< tai-seconds  (* (- 1972 1970) 365 sid)) 0
        (lsd  leap-second-table))))

(define-values (time make-time time? time-ref time-set!)
  (make-struct-type
   'time #f 3 0 #f
   (list (cons prop:serializable
               (make-serialize-info
                (lambda (t)
                  (vector (time-type t)
                          (time-nanosecond t)
                          (time-second t)))
                #'deserialize-info:time-v0
                #f
                (or (current-load-relative-directory)
                    (current-directory)))))
   (make-inspector) #f null))

(define deserialize-info:time-v0
  (make-deserialize-info
   make-time
   (lambda ()
     (let ((t0 (make-time #f #f #f)))
       (values t0 (lambda (t1)
                    (set-time-type!       t0 (time-type t1))
                    (set-time-nanosecond! t0 (time-nanosecond t1))
                    (set-time-second!     t0 (time-second t1))))))))

(define (time-type t)       (time-ref t 0))
(define (time-nanosecond t) (time-ref t 1))
(define (time-second t)     (time-ref t 2))

(define (set-time-type! t type)     (time-set! t 0 type))
(define (set-time-nanosecond! t ns) (time-set! t 1 ns))
(define (set-time-second! t s)      (time-set! t 2 s))

(define (copy-time time)
  (let ((ntime (make-time #f #f #f)))
    (set-time-type! ntime (time-type time))
    (set-time-second! ntime (time-second time))
    (set-time-nanosecond! ntime (time-nanosecond time))
    ntime))

(define (get-time-of-day)
  (let* ((total-msecs (inexact->exact (floor (current-inexact-milliseconds)))))
    (quotient/remainder total-msecs 1000)))

(define (current-time-utc)
  (receive (seconds ms) (get-time-of-day)
    (make-time time-utc (* ms 1000000) seconds)))

(define (current-time-tai)
  (receive (seconds ms) (get-time-of-day)
    (make-time time-tai
               (* ms 1000000)
               (+ seconds (leap-second-delta seconds)))))

(define (current-time-ms-time time-type proc)
  (let ((current-ms (proc)))
    (make-time time-type
               (* (remainder current-ms 1000) 1000000)
               (quotient current-ms 1000000))))

(define (current-time-monotonic)
  (receive (seconds ms) (get-time-of-day)
    (make-time time-monotonic
               (* ms 1000000)
               (+ seconds (leap-second-delta seconds)))))

(define (current-time-thread)
  (current-time-ms-time time-process current-process-milliseconds))

(define (current-time-process)
  (current-time-ms-time time-process current-process-milliseconds))

(define (current-time-gc)
  (current-time-ms-time time-gc current-gc-milliseconds))

(define (current-time (clock-type time-utc))
  (cond
    ((eq? clock-type time-tai) (current-time-tai))
    ((eq? clock-type time-utc) (current-time-utc))
    ((eq? clock-type time-monotonic) (current-time-monotonic))
    ((eq? clock-type time-thread) (current-time-thread))
    ((eq? clock-type time-process) (current-time-process))
    ((eq? clock-type time-gc) (current-time-gc))
    (else (time-error 'current-time 'invalid-clock-type clock-type))))

(define (time-resolution (clock-type time-utc))
  (cond
    ((eq? clock-type time-tai) 1000000)
    ((eq? clock-type time-utc) 1000000)
    ((eq? clock-type time-monotonic) 1000000)
    ((eq? clock-type time-thread) 1000000)
    ((eq? clock-type time-process) 1000000)
    ((eq? clock-type time-gc) 1000000)
    (else (time-error 'time-resolution 'invalid-clock-type clock-type))))

(define (time-compare-check time1 time2 caller)
  (if (or (not (and (time? time1) (time? time2)))
          (not (eq? (time-type time1) (time-type time2))))
      (time-error caller 'incompatible-time-types #f)
      #t))

(define (time=? time1 time2)
  (time-compare-check time1 time2 'time=?)
  (and (= (time-second time1) (time-second time2))
       (= (time-nanosecond time1) (time-nanosecond time2))))

(define (time>? time1 time2)
  (time-compare-check time1 time2 'time>?)
  (or (> (time-second time1) (time-second time2))
      (and (= (time-second time1) (time-second time2))
           (> (time-nanosecond time1) (time-nanosecond time2)))))

(define (time<? time1 time2)
  (time-compare-check time1 time2 'time<?)
  (or (< (time-second time1) (time-second time2))
      (and (= (time-second time1) (time-second time2))
           (< (time-nanosecond time1) (time-nanosecond time2)))))

(define (time>=? time1 time2)
  (time-compare-check time1 time2 'time>=?)
  (or (> (time-second time1) (time-second time2))
      (and (= (time-second time1) (time-second time2))
           (>= (time-nanosecond time1) (time-nanosecond time2)))))

(define (time<=? time1 time2)
  (time-compare-check time1 time2 'time<=?)
  (or (< (time-second time1) (time-second time2))
      (and (= (time-second time1) (time-second time2))
           (<= (time-nanosecond time1) (time-nanosecond time2)))))

(define (time->nanoseconds time)
  (define (sign1 n)
    (if (negative? n) -1 1))
  (+ (* (time-second time) nano)
     (time-nanosecond time)))

(define (nanoseconds->time time-type nanoseconds)
  (make-time time-type
             (remainder nanoseconds nano)
             (quotient nanoseconds nano)))

(define (nanoseconds->values nanoseconds)
  (values (abs (remainder nanoseconds nano))
          (quotient nanoseconds nano)))

(define time-difference
  (case-lambda
    ((time1 time2)
     (time-difference time1 time2 (make-time #f #f #f)))
    ((time1 time2 time3)
     (when (or (not (and (time? time1) (time? time2)))
               (not (eq? (time-type time1) (time-type time2))))
       (time-error 'time-difference 'incompatible-time-types #f))
     (set-time-type! time3 time-duration)
     (if (time=? time1 time2)
         (begin
           (set-time-second! time3 0)
           (set-time-nanosecond! time3 0))
         (receive
             (nanos secs)
             (nanoseconds->values (- (time->nanoseconds time1)
                                     (time->nanoseconds time2)))
           (set-time-second! time3 secs)
           (set-time-nanosecond! time3 nanos)))
     time3)))

(define (time-difference! time1 time2)
  (time-difference time1 time2 time1))

(define add-duration
  (case-lambda
    ((time1 duration)
     (add-duration time1 duration (make-time (time-type time1) #f #f)))
    ((time1 duration time3)
     (unless (and (time? time1) (time? duration))
       (time-error 'add-duration 'incompatible-time-types #f))
     (if (not (eq? (time-type duration) time-duration))
         (time-error 'add-duration 'not-duration duration)
         (let ((sec-plus (+ (time-second time1) (time-second duration)))
               (nsec-plus (+ (time-nanosecond time1) (time-nanosecond duration))))
           (let ((r (remainder nsec-plus nano))
                 (q (quotient nsec-plus nano)))
             ; (set-time-type! time3 (time-type time1))
             (if (negative? r)
                 (begin
                   (set-time-second! time3 (+ sec-plus q -1))
                   (set-time-nanosecond! time3 (+ nano r)))
                 (begin
                   (set-time-second! time3 (+ sec-plus q))
                   (set-time-nanosecond! time3 r)))
             time3))))))

(define (add-duration! time1 duration)
  (add-duration time1 duration time1))

(define subtract-duration
  (case-lambda
    ((time1 duration)
     (subtract-duration time1 duration (make-time (time-type time1) #f #f)))
    ((time1 duration time3)
     (unless (and (time? time1) (time? duration))
       (time-error 'add-duration 'incompatible-time-types #f))
     (if (not (eq? (time-type duration) time-duration))
         (time-error 'subtract-duration 'not-duration duration)
         (let ((sec-minus  (- (time-second time1) (time-second duration)))
               (nsec-minus (- (time-nanosecond time1) (time-nanosecond duration))))
           (let ((r (remainder nsec-minus nano))
                 (q (quotient nsec-minus nano)))
             (if (negative? r)
                 (begin
                   (set-time-second! time3 (- sec-minus q 1))
                   (set-time-nanosecond! time3 (+ nano r)))
                 (begin
                   (set-time-second! time3 (- sec-minus q))
                   (set-time-nanosecond! time3 r)))
             time3))))))

(define (subtract-duration! time1 duration)
  (subtract-duration time1 duration time1))


(define time-tai->time-utc!
  (case-lambda
    ((time-in)
     (time-tai->time-utc! time-in time-in 'time-tai->time-utc!))
    ((time-in time-out caller)
     (unless (eq? (time-type time-in) time-tai)
       (time-error caller 'incompatible-time-types time-in))
     (set-time-type! time-out time-utc)
     (set-time-nanosecond! time-out (time-nanosecond time-in))
     (set-time-second!     time-out (- (time-second time-in)
                                       (leap-second-neg-delta
                                        (time-second time-in))))
     time-out)))

(define (time-tai->time-utc time-in)
  (time-tai->time-utc! time-in (make-time #f #f #f) 'time-tai->time-utc))

(define time-utc->time-tai!
  (case-lambda
    ((time-in)
     (time-utc->time-tai! time-in time-in 'time-utc->time-tai!))
    ((time-in time-out caller)
     (unless (eq? (time-type time-in) time-utc)
       (time-error caller 'incompatible-time-types time-in))
     (set-time-type! time-out time-tai)
     (set-time-nanosecond! time-out (time-nanosecond time-in))
     (set-time-second!     time-out (+ (time-second time-in)
                                       (leap-second-delta
                                        (time-second time-in))))
     time-out)))

(define (time-utc->time-tai time-in)
  (time-utc->time-tai! time-in (make-time #f #f #f) 'time-utc->time-tai))

(define (time-monotonic->time-utc time-in)
  (unless (eq? (time-type time-in) time-monotonic)
    (time-error 'time-monotoinc->time-utc 'incompatible-time-types time-in))
  (let ((ntime (copy-time time-in)))
    (set-time-type! ntime time-tai)
    (time-tai->time-utc! ntime ntime 'time-monotonic->time-utc)))

(define (time-monotonic->time-utc! time-in)
  (unless (eq? (time-type time-in) time-monotonic)
    (time-error 'time-monotonic->time-utc! 'incompatible-time-types time-in))
  (set-time-type! time-in time-tai)
  (time-tai->time-utc! time-in time-in 'time-monotonic->time-utc))

(define (time-monotonic->time-tai time-in)
  (unless (eq? (time-type time-in) time-monotonic)
    (time-error 'time-monotonic->time-tai 'incompatible-time-types time-in))
  (let ((ntime (copy-time time-in)))
    (set-time-type! ntime time-tai)
    ntime))

(define (time-monotonic->time-tai! time-in)
  (unless (eq? (time-type time-in) time-monotonic)
    (time-error 'time-monotonic->time-tai! 'incompatible-time-types time-in))
  (set-time-type! time-in time-tai)
  time-in)

(define (time-utc->time-monotonic time-in)
  (unless (eq? (time-type time-in) time-utc)
    (time-error 'time-utc->time-monotonic 'incompatible-time-types time-in))
  (let ((ntime (time-utc->time-tai! time-in (make-time #f #f #f)
                                    'time-utc->time-monotonic)))
    (set-time-type! ntime time-monotonic)
    ntime))


(define (time-utc->time-monotonic! time-in)
  (unless (eq? (time-type time-in) time-utc)
    (time-error 'time-utc->time-montonic! 'incompatible-time-types time-in))
  (let ((ntime (time-utc->time-tai! time-in time-in
                                    'time-utc->time-monotonic!)))
    (set-time-type! ntime time-monotonic)
    ntime))


(define (time-tai->time-monotonic time-in)
  (unless (eq? (time-type time-in) time-tai)
    (time-error 'time-tai->time-monotonic 'incompatible-time-types time-in))
  (let ((ntime (copy-time time-in)))
    (set-time-type! ntime time-monotonic)
    ntime))

(define (time-tai->time-monotonic! time-in)
  (unless (eq? (time-type time-in) time-tai)
    (time-error 'time-tai->time-monotonic!  'incompatible-time-types time-in))
  (set-time-type! time-in time-monotonic)
  time-in)

(define (make-datetime nanosecond second minute
                       hour day month
                       year zone-offset)
  (with-handlers ((exn:fail:contract?
                   (thunk* (datetime* nanosecond second minute hour
                                      day month year zone-offset))))
    (date* second minute hour
           day month year
           (week-day day month year)
           (- (year-day day month year) 1)
           #f zone-offset nanosecond "")))

(define-struct datetime*
  (nanosecond second minute
              hour day month
              year zone-offset)
  #:transparent)

(define-syntax-rule (define-date-accessor accessor time-date-accessor date*-accessor)
  (define (accessor d)
    (if (datetime*? d)
        (time-date-accessor d)
        (date*-accessor d))))

(define-date-accessor datetime-nanosecond  datetime*-nanosecond  date*-nanosecond)
(define-date-accessor datetime-second  datetime*-second      date-second)
(define-date-accessor datetime-minute  datetime*-minute      date-minute)
(define-date-accessor datetime-hour    datetime*-hour        date-hour)
(define-date-accessor datetime-day     datetime*-day         date-day)
(define-date-accessor datetime-month   datetime*-month       date-month)
(define-date-accessor datetime-year    datetime*-year        date-year)
(define-date-accessor datetime-zone-offset datetime*-zone-offset date-time-zone-offset)

;;

(define (datetime*->date* date)
  (make-datetime (datetime*-nanosecond date)
                 (datetime*-second date)
                 (datetime*-minute date)
                 (datetime*-hour date)
                 (datetime*-day date)
                 (datetime*-month date)
                 (datetime*-year date)
                 (datetime*-zone-offset date)))

(define (datetime? d)
  (or (datetime*? d) (date? d)))

(define (encode-julian-day-number day month year)
  (let* ((a (quotient (- 14 month) 12))
         (y (- (+ year 4800) a (if (negative? year) -1  0)))
         (m (- (+ month (* 12 a)) 3)))
    (+ day
       (quotient (+ (* 153 m) 2) 5)
       (* 365 y)
       (quotient y 4)
       (- (quotient y 100))
       (quotient y 400)
       -32045)))

(define (char-pos char str index len)
  (cond
    ((>= index len) #f)
    ((char=? (string-ref str index) char)
     index)
    (else
     (char-pos char str (+ index 1) len))))

(define (decimal-expansion r precision)
  (let loop ((num (- r (round r)))
             (p precision))
    (if (or (= p 0) (= num 0))
        ""
        (let* ((num-times-10 (* 10 num))
               (round-num-times-10 (round num-times-10)))
          (string-append (number->string (inexact->exact round-num-times-10))
                         (loop (- num-times-10 round-num-times-10) (- p 1)))))))

(define (decode-julian-day-number jdn)
  (let* ((days (truncate jdn))
         (a (+ days 32044))
         (b (quotient (+ (* 4 a) 3) 146097))
         (c (- a (quotient (* 146097 b) 4)))
         (d (quotient (+ (* 4 c) 3) 1461))
         (e (- c (quotient (* 1461 d) 4)))
         (m (quotient (+ (* 5 e) 2) 153))
         (y (+ (* 100 b) d -4800 (quotient m 10))))
    (values ; seconds date month year
     (* (- jdn days) sid)
     (+ e (- (quotient (+ (* 153 m) 2) 5)) 1)
     (+ m 3 (* -12 (quotient m 10)))
     (if (>= 0 y) (- y 1) y))
    ))

(define (local-tz-offset)
  (date-time-zone-offset (seconds->date (current-seconds))))

(define (time->julian-day-number seconds tz-offset)
  (+ (/ (+ seconds
           tz-offset
           sihd)
        sid)
     tai-epoch-in-jd))

(define (find proc l)
  (if (null? l)
      #f
      (if (proc (car l))
          #t
          (find proc (cdr l)))))

(define (tai-before-leap-second? second)
  (find (lambda (x)
          (= second (- (+ (car x) (cdr x)) 1)))
        leap-second-table))

(define (time-tai->datetime time . tz-offset)
  (if (tai-before-leap-second? (time-second time))
      (let ((d (time->datetime (subtract-duration! (time-tai->time-utc time)
                                               (make-time time-duration 0 1))
                           tz-offset time-utc)))
        (make-datetime (datetime-nanosecond d)
                       60
                       (datetime-minute d)
                       (datetime-hour d)
                       (datetime-day d)
                       (datetime-month d)
                       (datetime-year d)
                       (datetime-zone-offset d)))
      (time->datetime (time-tai->time-utc time) tz-offset time-utc)))

(define (time-utc->datetime time . tz-offset)
  (time->datetime time tz-offset time-utc))

(define (time-monotonic->datetime time . tz-offset)
  (time->datetime time tz-offset time-monotonic))

(define (datetime->time-utc date)
  (let ((nanosecond (datetime-nanosecond date))
        (second (datetime-second date))
        (minute (datetime-minute date))
        (hour (datetime-hour date))
        (day (datetime-day date))
        (month (datetime-month date))
        (year (datetime-year date))
        (offset (datetime-zone-offset date)))
    (let ((jdays (- (encode-julian-day-number day month year)
                    tai-epoch-in-jd)))
      (make-time
       time-utc
       nanosecond
       (+ (* (- jdays 1/2) 24 60 60)
          (* hour 60 60)
          (* minute 60)
          second
          (- offset))
       ))))

(define (datetime->time-tai d)
  (if (= (datetime-second d) 60)
      (subtract-duration! (time-utc->time-tai! (datetime->time-utc d)) (make-time time-duration 0 1))
      (time-utc->time-tai! (datetime->time-utc d))))

(define (datetime->time-monotonic date)
  (time-utc->time-monotonic! (datetime->time-utc date)))

(define leap-year?
  (match-lambda
    ((app number? year)
     (or (= (modulo year 400) 0)
         (and (= (modulo year 4) 0) (not (= (modulo year 100) 0)))))
    ((app datetime? date)
     (leap-year? (datetime-year date)))))

(define month-assoc '((0 . 0)
                      (1 . 31)
                      (2 . 59)
                      (3 . 90)
                      (4 . 120)
                      (5 . 151)
                      (6 . 181)
                      (7 . 212)
                      (8 . 243)
                      (9 . 273)
                      (10 . 304)
                      (11 . 334)))

(define (year-day day month year)
  (let ((days-pr (assoc (- month 1) month-assoc)))
    (unless days-pr
      (time-error 'date-year-day 'invalid-month-specification month))
    (if (and (leap-year? year) (> month 2))
        (+ day (cdr days-pr) 1)
        (+ day (cdr days-pr)))))

(define (datetime-year-day date)
  (year-day (datetime-day date) (datetime-month date) (datetime-year date)))

(define (week-day day month year)
  (let* ((a (quotient (- 14 month) 12))
         (y (- year a))
         (m (+ month (* 12 a) -2)))
    (modulo (+ day y (quotient y 4) (- (quotient y 100))
               (quotient y 400) (quotient (* 31 m) 12))
            7)))

(define (datetime-week-day date)
  (week-day (datetime-day date) (datetime-month date) (datetime-year date)))

(define (days-before-first-week date day-of-week-starting-week)
  (let* ((first-day (make-datetime 0 0 0 0
                                   1
                                   1
                                   (datetime-year date)
                                   #f))
         (fdweek-day (datetime-week-day first-day))  )
    (modulo (- day-of-week-starting-week fdweek-day)
            7)))

(define (date-week-number date day-of-week-starting-week)
  (quotient (- (datetime-year-day date)
               (days-before-first-week  date day-of-week-starting-week))
            7))

(define (current-date . tz-offset)
  (time-utc->datetime (current-time time-utc)
                      (:optional tz-offset (local-tz-offset))))

(define (natural-year n)
  (let* ((current-year (datetime-year (current-date)))
         (current-century (* (quotient current-year 100) 100)))
    (cond
      ((>= n 100) n)
      ((<  n 0) n)
      ((<=  (- (+ current-century n) current-year) 50)
       (+ current-century n))
      (else
       (+ (- current-century 100) n)))))

(define (datetime->julian-day date)
  (let ((nanosecond (datetime-nanosecond date))
        (second (datetime-second date))
        (minute (datetime-minute date))
        (hour (datetime-hour date))
        (day (datetime-day date))
        (month (datetime-month date))
        (year (datetime-year date))
        (offset (datetime-zone-offset date)))
    (+ (encode-julian-day-number day month year)
       (- 1/2)
       (+ (/ (+ (* hour 60 60)
                (* minute 60)
                second
                (/ nanosecond nano)
                (- offset))
             sid)))))

(define (datetime->modified-julian-day date)
  (- (datetime->julian-day date)
     4800001/2))


(define (time-utc->julian-day time)
  (unless (eq? (time-type time) time-utc)
    (time-error 'time->datetime 'incompatible-time-types  time))
  (+ (/ (+ (time-second time) (/ (time-nanosecond time) nano))
        sid)
     tai-epoch-in-jd))

(define (time-utc->modified-julian-day time)
  (- (time-utc->julian-day time)
     4800001/2))

(define (time-tai->julian-day time)
  (unless (eq? (time-type time) time-tai)
    (time-error 'time->datetime 'incompatible-time-types  time))
  (+ (/ (+ (- (time-second time)
              (leap-second-delta (time-second time)))
           (/ (time-nanosecond time) nano))
        sid)
     tai-epoch-in-jd))

(define (time-tai->modified-julian-day time)
  (- (time-tai->julian-day time)
     4800001/2))

(define (time-monotonic->julian-day time)
  (unless (eq? (time-type time) time-monotonic)
    (time-error 'time->datetime 'incompatible-time-types  time))
  (+ (/ (+ (- (time-second time)
              (leap-second-delta (time-second time)))
           (/ (time-nanosecond time) nano))
        sid)
     tai-epoch-in-jd))


(define (time-monotonic->modified-julian-day time)
  (- (time-monotonic->julian-day time)
     4800001/2))


(define (julian-day->time-utc jdn)
  (let ((nanosecs (* nano sid (- jdn tai-epoch-in-jd))))
    (make-time time-utc
               (remainder nanosecs nano)
               (floor (/ nanosecs nano)))))

(define (julian-day->time-tai jdn)
  (time-utc->time-tai! (julian-day->time-utc jdn)))

(define (julian-day->time-monotonic jdn)
  (time-utc->time-monotonic! (julian-day->time-utc jdn)))

(define (julian-day->datetime jdn . tz-offset)
  (let ((offset (:optional tz-offset (local-tz-offset))))
    (time-utc->datetime (julian-day->time-utc jdn) offset)))

(define (modified-julian-day->datetime jdn . tz-offset)
  (let ((offset (:optional tz-offset (local-tz-offset))))
    (julian-day->datetime (+ jdn 4800001/2) offset)))

(define (modified-julian-day->time-utc jdn)
  (julian-day->time-utc (+ jdn 4800001/2)))

(define (modified-julian-day->time-tai jdn)
  (julian-day->time-tai (+ jdn 4800001/2)))

(define (modified-julian-day->time-monotonic jdn)
  (julian-day->time-monotonic (+ jdn 4800001/2)))

(define (current-julian-day)
  (time-utc->julian-day (current-time time-utc)))

(define (current-modified-julian-day)
  (time-utc->modified-julian-day (current-time time-utc)))

(define (padding n pad-with length)
  (let* ((str (number->string n))
         (str-len (string-length str)))
    (if (or (> str-len length)
            (not pad-with))
        str
        (let* ((new-str (make-string length pad-with))
               (new-str-offset (- (string-length new-str)
                                  str-len)))
          (do ((i 0 (+ i 1)))
              ((>= i (string-length str)))
            (string-set! new-str (+ new-str-offset i)
                         (string-ref str i)))
          new-str))))

(define (last-n-digits i n)
  (abs (remainder i (expt 10 n))))

(define (locale-abbr-weekday n)
  (localized-message (vector-ref locale-abbr-weekday-vector n)))

(define (locale-long-weekday n)
  (localized-message (vector-ref locale-long-weekday-vector n)))

(define (locale-abbr-month n)
  (localized-message (vector-ref locale-abbr-month-vector (- n 1))))

(define (locale-long-month n)
  (localized-message (vector-ref locale-long-month-vector (- n 1))))

(define (vector-find needle haystack comparator)
  (let ((len (vector-length haystack)))
    (define (vector-find-int index)
      (cond
        ((>= index len) #f)
        ((comparator needle (localized-message (vector-ref haystack index))) (+ index 1))
        (else (vector-find-int (+ index 1)))))
    (vector-find-int 0)))

(define (locale-abbr-weekday->index string)
  (vector-find string locale-abbr-weekday-vector string=?))

(define (locale-long-weekday->index string)
  (vector-find string locale-long-weekday-vector string=?))

(define (locale-abbr-month->index string)
  (vector-find string locale-abbr-month-vector string=?))

(define (locale-long-month->index string)
  (vector-find string locale-long-month-vector string=?))

(define (locale-print-time-zone date port)
  (values))

(define (locale-am/pm hr)
  (localized-message
   (if (> hr 11) locale-pm locale-am)))

(define (tz-printer offset port)
  (display
   (cond ((= offset 0) "Z")
         (else (let ((sign (cond ((negative? offset) "-")
                                 (else               "+")))
                     (hours (abs (quotient offset (* 60 60))))
                     (minutes (abs (quotient (remainder offset (* 60 60)) 60))))
                 (string-append sign (padding hours #\0 2) (padding minutes #\0 2)))))
   port))

(define directives
  (list
   (cons #\~ (lambda (date pad-with port) (display #\~ port)))

   (cons #\a (lambda (date pad-with port)
               (display (locale-abbr-weekday (datetime-week-day date))
                        port)))
   (cons #\A (lambda (date pad-with port)
               (display (locale-long-weekday (datetime-week-day date))
                        port)))
   (cons #\b (lambda (date pad-with port)
               (display (locale-abbr-month (datetime-month date))
                        port)))
   (cons #\B (lambda (date pad-with port)
               (display (locale-long-month (datetime-month date))
                        port)))
   (cons #\c (lambda (date pad-with port)
               (display (datetime->string date (localized-message locale-date-time-format)) port)))
   (cons #\d (lambda (date pad-with port)
               (display (padding (datetime-day date)
                                 #\0 2)
                        port)))
   (cons #\D (lambda (date pad-with port)
               (display (datetime->string date "~m/~d/~y") port)))
   (cons #\e (lambda (date pad-with port)
               (display (padding (datetime-day date)
                                 #\Space 2)
                        port)))
   (cons #\f (lambda (date pad-with port)
               (if (> (datetime-nanosecond date)
                      nano)
                   (display (padding (+ (datetime-second date) 1)
                                     pad-with 2)
                            port)
                   (display (padding (datetime-second date)
                                     pad-with 2)
                            port))
               (let ((f (decimal-expansion (/ (datetime-nanosecond date) nano) 6)))
                 (when (> (string-length f) 0)
                   (display (localized-message locale-number-separator) port)
                   (display f port)))))
   (cons #\h (lambda (date pad-with port)
               (display (datetime->string date "~b") port)))
   (cons #\H (lambda (date pad-with port)
               (display (padding (datetime-hour date)
                                 pad-with 2)
                        port)))
   (cons #\I (lambda (date pad-with port)
               (let ((hr (datetime-hour date)))
                 (if (> hr 12)
                     (display (padding (- hr 12)
                                       pad-with 2)
                              port)
                     (display (padding hr
                                       pad-with 2)
                              port)))))
   (cons #\j (lambda (date pad-with port)
               (display (padding (datetime-year-day date)
                                 pad-with 3)
                        port)))
   (cons #\k (lambda (date pad-with port)
               (display (padding (datetime-hour date)
                                 #\0 2)
                        port)))
   (cons #\l (lambda (date pad-with port)
               (let ((hr (if (> (datetime-hour date) 12)
                             (- (datetime-hour date) 12) (datetime-hour date))))
                 (display (padding hr  #\Space 2)
                          port))))
   (cons #\m (lambda (date pad-with port)
               (display (padding (datetime-month date)
                                 pad-with 2)
                        port)))
   (cons #\M (lambda (date pad-with port)
               (display (padding (datetime-minute date)
                                 pad-with 2)
                        port)))
   (cons #\n (lambda (date pad-with port)
               (newline port)))
   (cons #\N (lambda (date pad-with port)
               (display (padding (datetime-nanosecond date)
                                 pad-with 9)
                        port)))
   (cons #\p (lambda (date pad-with port)
               (display (locale-am/pm (datetime-hour date)) port)))
   (cons #\r (lambda (date pad-with port)
               (display (datetime->string date "~I:~M:~S ~p") port)))
   (cons #\s (lambda (date pad-with port)
               (display (time-second (datetime->time-utc date)) port)))
   (cons #\S (lambda (date pad-with port)
               (if (> (datetime-nanosecond date)
                      nano)
                   (display (padding (+ (datetime-second date) 1)
                                     pad-with 2)
                            port)
                   (display (padding (datetime-second date)
                                     pad-with 2)
                            port))))
   (cons #\t (lambda (date pad-with port)
               (display #\Tab port)))
   (cons #\T (lambda (date pad-with port)
               (display (datetime->string date "~H:~M:~S") port)))
   (cons #\U (lambda (date pad-with port)
               (if (> (days-before-first-week date 0) 0)
                   (display (padding (+ (date-week-number date 0) 1)
                                     #\0 2) port)
                   (display (padding (date-week-number date 0)
                                     #\0 2) port))))
   (cons #\V (lambda (date pad-with port)
               (display (padding (date-week-number date 1)
                                 #\0 2) port)))
   (cons #\w (lambda (date pad-with port)
               (display (datetime-week-day date) port)))
   (cons #\x (lambda (date pad-with port)
               (display (datetime->string date (localized-message locale-short-date-format)) port)))
   (cons #\X (lambda (date pad-with port)
               (display (datetime->string date (localized-message locale-time-format)) port)))
   (cons #\W (lambda (date pad-with port)
               (if (> (days-before-first-week date 1) 0)
                   (display (padding (+ (date-week-number date 1) 1)
                                     #\0 2) port)
                   (display (padding (date-week-number date 1)
                                     #\0 2) port))))
   (cons #\y (lambda (date pad-with port)
               (display (padding (last-n-digits
                                  (datetime-year date) 2)
                                 pad-with
                                 2)
                        port)))
   (cons #\Y (lambda (date pad-with port)
               (display (datetime-year date) port)))
   (cons #\z (lambda (date pad-with port)
               (tz-printer (datetime-zone-offset date) port)))
   (cons #\Z (lambda (date pad-with port)
               (locale-print-time-zone date port)))
   (cons #\1 (lambda (date pad-with port)
               (display (datetime->string date "~Y-~m-~d") port)))
   (cons #\2 (lambda (date pad-with port)
               (display (datetime->string date "~k:~M:~S~z") port)))
   (cons #\3 (lambda (date pad-with port)
               (display (datetime->string date "~k:~M:~S") port)))
   (cons #\4 (lambda (date pad-with port)
               (display (datetime->string date "~Y-~m-~dT~k:~M:~S~z") port)))
   (cons #\5 (lambda (date pad-with port)
               (display (datetime->string date "~Y-~m-~dT~k:~M:~S") port)))
   ))


(define (get-formatter char)
  (let ((associated (assoc char directives)))
    (if associated (cdr associated) #f)))

(define (date-printer date index format-string str-len port)
  (if (>= index str-len)
      (values)
      (let ((current-char (string-ref format-string index)))
        (if (not (char=? current-char #\~))
            (begin
              (display current-char port)
              (date-printer date (+ index 1) format-string str-len port))
            (if (= (+ index 1) str-len) ; bad format string.
                (time-error 'date-printer 'bad-date-format-string
                            format-string)
                (let ((pad-char? (string-ref format-string (+ index 1))))
                  (cond
                    ((char=? pad-char? #\-)
                     (if (= (+ index 2) str-len) ; bad format string.
                         (time-error 'date-printer 'bad-date-format-string
                                     format-string)
                         (let ((formatter (get-formatter
                                           (string-ref format-string
                                                       (+ index 2)))))
                           (if (not formatter)
                               (time-error 'date-printer 'bad-date-format-string
                                           format-string)
                               (begin
                                 (formatter date #f port)
                                 (date-printer date (+ index 3)
                                               format-string str-len port))))))

                    ((char=? pad-char? #\_)
                     (if (= (+ index 2) str-len) ; bad format string.
                         (time-error 'date-printer 'bad-date-format-string
                                     format-string)
                         (let ((formatter (get-formatter
                                           (string-ref format-string
                                                       (+ index 2)))))
                           (if (not formatter)
                               (time-error 'date-printer 'bad-date-format-string
                                           format-string)
                               (begin
                                 (formatter date #\Space port)
                                 (date-printer date (+ index 3)
                                               format-string str-len port))))))
                    (else
                     (let ((formatter (get-formatter
                                       (string-ref format-string
                                                   (+ index 1)))))
                       (if (not formatter)
                           (time-error 'date-printer 'bad-date-format-string
                                       format-string)
                           (begin
                             (formatter date #\0 port)
                             (date-printer date (+ index 2)
                                           format-string str-len port))))))))))))


(define (datetime->string date (format-string "~c"))
  (unless (string? format-string)
    (raise-type-error 'datetime->string "string" 1 date format-string))
  (let ((str-port (open-output-string)))
    (date-printer date 0 format-string (string-length format-string) str-port)
    (get-output-string str-port)))

(define (char->int ch)
  (cond
    ((char=? ch #\0) 0)
    ((char=? ch #\1) 1)
    ((char=? ch #\2) 2)
    ((char=? ch #\3) 3)
    ((char=? ch #\4) 4)
    ((char=? ch #\5) 5)
    ((char=? ch #\6) 6)
    ((char=? ch #\7) 7)
    ((char=? ch #\8) 8)
    ((char=? ch #\9) 9)
    (else (time-error 'bad-date-template-string
                      'digit-char
                      ch))))

(define (integer-reader upto port)
  (define (accum-int port accum nchars)
    (let ((ch (peek-char port)))
      (if (or (eof-object? ch)
              (not (char-numeric? ch))
              (and upto (>= nchars  upto )))
          accum
          (accum-int port (+ (* accum 10) (char->int (read-char port))) (+ nchars 1)))))
  (accum-int port 0 0))

(define (make-integer-reader upto)
  (lambda (port)
    (integer-reader upto port)))

(define (fractional-integer-reader upto port)
  (define (accum-int port accum nchars)
    (let ((ch (peek-char port)))
      (if (or (eof-object? ch)
              (not (char-numeric? ch))
              (and upto (>= nchars  upto )))
          (* accum (expt 10 (- upto nchars)))
          (accum-int port (+ (* accum 10) (char->int (read-char port))) (+ nchars 1)))))
  (accum-int port 0 0))

(define (make-fractional-integer-reader upto)
  (lambda (port)
    (fractional-integer-reader upto port)))

(define (integer-reader-exact n port)
  (let ((padding-ok #t))
    (define (accum-int port accum nchars)
      (let ((ch (peek-char port)))
        (cond
          ((>= nchars n) accum)
          ((eof-object? ch)
           (time-error 'string->datetime 'bad-date-template-string
                       "premature ending to integer read."))
          ((char-numeric? ch)
           (set! padding-ok #f)
           (accum-int port (+ (* accum 10) (char->int (read-char port)))
                      (+ nchars 1)))
          (padding-ok
           (read-char port)             ; consume padding
           (accum-int port accum (+ nchars 1)))
          (else                 ; padding where it shouldn't be
           (time-error 'string->datetime 'bad-date-template-string
                       "non-numeric characters in integer read.")))))
    (accum-int port 0 0)))


(define (make-integer-exact-reader n)
  (lambda (port)
    (integer-reader-exact n port)))

(define (zone-reader port)
  (let ((offset 0)
        (positive? #f))
    (let ((ch (read-char port)))
      (when (eof-object? ch)
        (time-error 'string->datetime 'bad-date-template-string
                    (list "invalid time zone +/-" ch)))
      (if (or (char=? ch #\Z) (char=? ch #\z))
          0
          (begin
            (cond
              ((char=? ch #\+) (set! positive? #t))
              ((char=? ch #\-) (set! positive? #f))
              (else
               (time-error 'string->datetime 'bad-date-template-string
                           (list "invalid time zone +/-" ch))))
            (let ((ch (read-char port)))
              (when (eof-object? ch)
                (time-error 'string->datetime 'bad-date-template-string (list "invalid time zone number" ch)))
              (set! offset (* (char->int ch)
                              10 60 60)))
            (let ((ch (read-char port)))
              (unless (eof-object? ch)
                ;; FIXME: non-existing values should be considered Zero instead of an error
                ;; (time-error 'string->datetime 'bad-date-template-string (list "invalid time zone number" ch)))
                (set! offset (+ offset (* (char->int ch) 60 60)))))
            (let ((ch (read-char port)))
              (unless (eof-object? ch)
                ;; FIXME: non-existing values should be considered Zero instead of an error
                ;; (time-error 'string->datetime 'bad-date-template-string (list "invalid time zone number" ch)))
                (set! offset (+ offset (* (char->int ch) 10 60)))))
            (let ((ch (read-char port)))
              (unless (eof-object? ch)
                ;; FIXME: non-existing values should be considered Zero instead of an error
                ;; (time-error 'string->datetime 'bad-date-template-string (list "invalid time zone number" ch)))
                (set! offset (+ offset (* (char->int ch) 60)))))
            (if positive? offset (- offset)))))))

(define (locale-reader port indexer)
  (let ((string-port (open-output-string)))
    (define (read-char-string)
      (let ((ch (peek-char port)))
        (if (char-alphabetic? ch)
            (begin (write-char (read-char port) string-port)
                   (read-char-string))
            (get-output-string string-port))))
    (let* ((str (read-char-string))
           (index (indexer str)))
      (if index index (time-error 'string->datetime
                                  'bad-date-template-string
                                  (list "invalid string for " indexer))))))

(define (make-locale-reader indexer)
  (lambda (port)
    (locale-reader port indexer)))

(define (make-char-id-reader char)
  (lambda (port)
    (if (char=? char (read-char port))
        char
        (time-error 'string->datetime
                    'bad-date-template-string
                    "invalid character match."))))

(define read-directives
  (let ((ireader4 (make-integer-reader 4))
        (ireader2 (make-integer-reader 2))
        (fireader9 (make-fractional-integer-reader 9))
        (ireaderf (make-integer-reader #f))
        (eireader2 (make-integer-exact-reader 2))
        (eireader4 (make-integer-exact-reader 4))
        (locale-reader-abbr-weekday (make-locale-reader
                                     locale-abbr-weekday->index))
        (locale-reader-long-weekday (make-locale-reader
                                     locale-long-weekday->index))
        (locale-reader-abbr-month   (make-locale-reader
                                     locale-abbr-month->index))
        (locale-reader-long-month   (make-locale-reader
                                     locale-long-month->index))
        (char-fail (lambda (ch) #t))
        (do-nothing (lambda (val object) object))
        )

    (list
     (list #\~ char-fail (make-char-id-reader #\~) do-nothing)
     (list #\a char-alphabetic? locale-reader-abbr-weekday do-nothing)
     (list #\A char-alphabetic? locale-reader-long-weekday do-nothing)
     (list #\b char-alphabetic? locale-reader-abbr-month
           (lambda (val object)
             (struct-copy datetime* object (month val))))
     (list #\B char-alphabetic? locale-reader-long-month
           (lambda (val object)
             (struct-copy datetime* object (month val))))
     (list #\d char-numeric? ireader2
           (lambda (val object)
             (struct-copy datetime* object (day val))))
     (list #\e char-fail eireader2
           (lambda (val object)
             (struct-copy datetime* object (day val))))
     (list #\h char-alphabetic? locale-reader-abbr-month
           (lambda (val object)
             (struct-copy datetime* object (month val))))
     (list #\H char-numeric? ireader2
           (lambda (val object)
             (struct-copy datetime* object (hour val))))
     (list #\k char-fail eireader2
           (lambda (val object)
             (struct-copy datetime* object (hour val))))
     (list #\m char-numeric? ireader2
           (lambda (val object)
             (struct-copy datetime* object (month val))))
     (list #\M char-numeric? ireader2
           (lambda (val object)
             (struct-copy datetime* object (minute val))))
     (list #\N char-numeric? fireader9
           (lambda (val object)
             (struct-copy datetime* object (nanosecond val))))
     (list #\S char-numeric? ireader2
           (lambda (val object)
             (struct-copy datetime* object (second val))))
     (list #\y char-fail eireader2
           (lambda (val object)
             (struct-copy datetime* object
                          (year (natural-year val)))))
     (list #\Y char-numeric? ireader4
           (lambda (val object)
             (struct-copy datetime* object (year val))))
     (list #\z (lambda (c)
                 (or (char=? c #\Z)
                     (char=? c #\z)
                     (char=? c #\+)
                     (char=? c #\-)))
           zone-reader
           (lambda (val object)
             (struct-copy datetime* object (zone-offset val))))
     ; PLT-specific extension for 2- or 4-digit years:
     (list #\? char-numeric? ireader4
           (lambda (val object)
             (struct-copy datetime* object
                          (year (natural-year val)))))
     )))

(define string->datetime
  (case-lambda
    ((date index format-string str-len port template-string)
     (define (skip-until port skipper)
       (let ((ch (peek-char port)))
         (if (eof-object? ch)
             (time-error 'string->datetime 'bad-date-format-string template-string)
             (unless (skipper ch)
               (read-char port)
               (skip-until port skipper)))))
     (if (>= index str-len)
         date
         (let ((current-char (string-ref format-string index)))
           (if (not (char=? current-char #\~))
               (let ((port-char (read-char port)))
                 (when (or (eof-object? port-char)
                           (not (char=? current-char port-char)))
                   (time-error 'string->datetime 'bad-date-format-string template-string))
                 (string->datetime date (+ index 1) format-string str-len port template-string))
               (if (> (+ index 1) str-len)
                   (time-error 'string->datetime 'bad-date-format-string template-string)
                   (let* ((format-char (string-ref format-string (+ index 1)))
                          (format-info (assoc format-char read-directives)))
                     (if (not format-info)
                         (time-error 'string->datetime 'bad-date-format-string template-string)
                         (begin
                           (let ((skipper (cadr format-info))
                                 (reader  (caddr format-info))
                                 (actor   (cadddr format-info)))
                             (skip-until port skipper)
                             (define new-date
                               (let ((val (reader port)))
                                 (if (eof-object? val)
                                     (time-error 'string->datetime 'bad-date-format-string template-string)
                                     (actor val date))))
                             (string->datetime new-date (+ index 2) format-string str-len port template-string))))))))))
    ((input-string template-string)
     (define (date-ok? date)
       (and (datetime-nanosecond date)
            (datetime-second date)
            (datetime-minute date)
            (datetime-hour date)
            (datetime-day date)
            (datetime-month date)
            (datetime-year date)
            (datetime-zone-offset date)))
     (let* ((current (current-date))
            (initial (datetime* 0 0 0 0 #t #t (datetime-year current) (local-tz-offset)))
            (newdate (string->datetime
                      initial
                      0
                      template-string
                      (string-length template-string)
                      (open-input-string input-string)
                      template-string)))
       (if (date-ok? newdate)
           (datetime*->date* newdate)
           (time-error 'string->datetime 'bad-date-format-string (list "incomplete date read. " newdate template-string)))))))

;;

(define time-iso8601      "~Y-~m-~dT~H:~M:~S~z")    ;; 2006-01-02T15:04:05-0700
(define time-iso8601-nano "~Y-~m-~dT~H:~M:~S.~N~z") ;; 2006-01-02T15:04:05.999999999-0700

;;

(define nanosecond   1)
(define microsecond  1000)
(define millisecond (* 1000 1000))
(define second      (* 1000 1000 1000))
(define minute      (* 60 second))
(define hour        (* 60 minute))
(define day         (* 24 hour))
(define week        (* 7 day))

;;

(define (make-duration i unit)
  (let ((ns (* i unit))
	(s 0))
    (when (or (<= -999999999 ns)
	      (<= ns 999999999))
      (set! s  (quotient ns second))
      (set! ns (remainder ns second)))
    (make-time time-duration ns s)))

(define (nanosecond-duration i)  (make-duration i nanosecond))
(define (microsecond-duration i) (make-duration i microsecond))
(define (millisecond-duration i) (make-duration i millisecond))
(define (second-duration i)      (make-duration i second))
(define (minute-duration i)      (make-duration i minute))
(define (hour-duration i)        (make-duration i hour))
(define (day-duration i)         (make-duration i day))
(define (week-duration i)        (make-duration i week))

(define duration-units
  `((w  . ,week)
    (d  . ,day)
    (h  . ,hour)
    (m  . ,minute)
    (s  . ,second)
    (ms . ,millisecond)
    (µs . ,microsecond)
    (ns . ,nanosecond)))

(define (string->duration s)
  (define signs  '(#\+ #\-))
  (define digits '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0))
  (define units  '(#\w #\d #\h #\m #\s #\n #\µ))
  ;;
  (define (flush acc amount unit)
    (if (and (pair? amount)
	     (pair? unit))
        (cons (cons amount unit) acc)
        acc))
  (define (tokenize s)
    (let loop ((input   (string->list s))
	       (n        0)
	       (acc    '())
	       (amount '())
	       (unit   '()))
      (if (pair? input)
          (let ((c (car input)))
            (cond
              ((memq c units)
               (loop (cdr input) (+ n 1)
                     acc
                     amount
                     (cons (string->symbol (string c)) unit)))
              ((memq c digits)
               (let ((next-acc (flush acc amount unit)))
                 (unless (eq? acc next-acc)
                   (set! acc next-acc)
                   (set! amount '())
                   (set! unit   '()))
                 (loop (cdr input) (+ n 1)
                       acc
                       (cons (string->number (string c)) amount)
                       unit)))
              ((memq c signs)
               (when (> n 0)
                 (error "unexpected duration sign at" (+ n 1)))
               (loop (cdr input) (+ n 1)
                     (cons (string->symbol (string c)) acc)
                     amount
                     unit))
              (else (error (format
                            "unexpected token ~s at ~a"
                            (string c) (+ n 1))))))
          (flush acc amount unit))))
  (define (normalize tree)
    (let loop ((acc null)
	       (rest tree))
      (if (pair? rest)
          (let ((node (car rest)))
            (loop
             (cons (if (pair? node)
                       (let ((amount (car node))
                             (unit   (cdr node)))
                         (cons
                          (let normalize-amount ((acc 0) (m 1) (rest amount))
                            (cond
                              ((pair? rest)
                               (normalize-amount (+ acc (* m (car rest)))
                                                 (* m 10)
                                                 (cdr rest)))
                              (else acc)))
                          (let normalize-unit ((acc "") (rest unit))
                            (cond
                              ((pair? rest)
                               (normalize-unit (cons (symbol->string (car rest)) acc)
                                               (cdr rest)))
                              (else (string->symbol (string-join acc "")))))))
                       node)
                   acc)
             (cdr rest)))
          acc)))
  (define (fold tree)
    (if (pair? tree)
        (let ((sign (let ((fst (car tree)))
                      (if (and (symbol? fst) (eq? fst '-))
                          (begin0 -1 (set! tree (cdr tree)))
                          +1)))
              (amount (let loop ((acc 0) (rest tree))
                        (if (pair? rest)
                            (let* ((unit (cdar rest))
                                   (ns   (cdr (or (assoc unit duration-units)
                                                  (error (format "unexpected duration unit ~s" unit))))))
                              (loop (+ acc (* (caar rest) ns))
                                    (cdr rest)))
                            acc))))
          (make-duration (* sign amount) nanosecond))
        (make-duration 0 nanosecond)))
  ;;
  (fold (normalize (tokenize s))))

;; (string->duration "1m6s15ms10µs666ns")  ;; => #<time #77 type: time-duration nanosecond: 15010666 second: 66>
;; (string->duration "-1m6s15ms10µs666ns") ;; => #<time #77 type: time-duration nanosecond: -15010666 second: -66>

(define (duration->string t)
  (unless (eq? (time-type t) time-duration)
    (error (format "expected time-duration type, got: ~a"
		   (time-type t))))
  (let* ((timestamp (time->unix-nano t))
	 (duration-str (let loop ((units  duration-units)
				  (acc    "")
				  (ns    (abs timestamp)))
			 (if (and (pair? units)
				  (> ns 0))
                             (let* ((unit-ns (cdar units))
                                    (unit    (quotient ns unit-ns)))
                               (if (> unit 0)
                                   (loop (cdr units)
                                         (string-append acc
                                                        (number->string unit)
                                                        (symbol->string (caar units)))
                                         (remainder ns unit-ns))
                                   (loop (cdr units) acc ns)))
                             acc))))
    (if (negative? timestamp)
        (string-append "-" duration-str)
        duration-str)))

;; (duration->string (string->duration "1m6s15ms10µs666ns"))           ;; => 1m6s15ms10µs666ns
;; (duration->string (string->duration "01m006s015ms00010µs000666ns")) ;; => 1m6s15ms10µs666ns
;; (duration->string (string->duration "-1m6s15ms10µs666ns"))          ;; => -1m6s15ms10µs666ns

;;

(define (unix-microseconds unix) (exact->inexact (/ unix microsecond)))
(define (unix-milliseconds unix) (exact->inexact (/ unix millisecond)))
(define (unix-seconds unix)      (exact->inexact (/ unix second)))

;;

(define (time->time-utc time)
  (case (time-type time)
    ((time-utc)        time)
    ((time-tai)       (time-tai->time-utc time))
    ((time-monotonic) (time-monotonic->time-utc time))
    (else (error "unsupported time type" time))))

(define (time->unix-nano time)
  (unless (member (time-type time) '(time-utc time-duration))
    (set! time (time->time-utc time)))
  (+ (time-nanosecond time)
     (* second (time-second time))))

;;

(define time->datetime
  (case-lambda
    ((time tz-offset ttype)
     (unless (eq? (time-type time) ttype)
       (time-error 'time->datetime 'incompatible-time-types  time))
     (let* ((offset (:optional tz-offset (local-tz-offset))))
       (receive (secs date month year)
           (decode-julian-day-number
            (time->julian-day-number (time-second time) offset))
         (let* ((hours    (quotient secs (* 60 60)))
                (rem      (remainder secs (* 60 60)))
                (minutes  (quotient rem 60))
                (seconds  (remainder rem 60)))
           (make-datetime (time-nanosecond time)
                          seconds
                          minutes
                          hours
                          date
                          month
                          year
                          offset)))))
    ((time . tz-offset)
     (apply
      (case (time-type time)
        ((time-utc)       time-utc->datetime)
        ((time-tai)       time-tai->datetime)
        ((time-monotonic) time-monotonic->datetime)
        (else (error "unsupported time type" time)))
      time tz-offset))))

(define (datetime->time date) (datetime->time-utc date))

;;

(define (time->string time . format-str)
  (apply datetime->string (time->datetime time) format-str))

(define (string->time str format-str)
  (datetime->time (string->datetime str format-str)))

(module+ test
  (test-case "string->datetime->string"
    (check-equal?
     (datetime->string (string->datetime "Jan 25 18:25:53 2026" "~b ~d ~H:~M:~S ~Y"))
     "Sun Jan 25 18:25:53Z 2026")))
