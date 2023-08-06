#lang racket
(require srfi/19
         "constant.rkt"
         "parse.rkt")
(provide clickhouse-convert
         clickhouse-transition)

(define (clickhouse-transition from)
  (match from
    ((? clickhouse-number?) 'number)
    ((? clickhouse-string?) 'string)
    ('Date 'date)
    ('DateTime 'date)
    ((cons 'Array subtype) `(list ,(clickhouse-transition (car subtype))))
    ((cons 'Tuple subtypes) `(list ,@(map clickhouse-transition subtypes)))))

;; FIXME: Too much levels of (match), could we do better?
(define (clickhouse-convert from to v)
  (match from
    ((? clickhouse-number?)
     (match to
       ('string (match v
                  ((? string?) v)
                  ((? number?) (number->string v))))
       ('number (match v
                  ((? number?) v)
                  ((? string?) (string->number v))))
       ('date (match v
                ((? number?) (seconds->date v))
                ((? string?) (seconds->date (string->number v)))))))
    ((? clickhouse-string?)
     (match to
       ('string (match v
                  ((? string?) v)
                  ((? number?) (number->string v))))
       ('number (match v
                  ((? number?) v)
                  ((? string?) (string->number v))))))
    ((cons 'Array from-subtype)
     (match to
       ((cons 'list to-subtype)
        (map (lambda (v)
               (clickhouse-convert
                (car from-subtype)
                (car to-subtype)
                v))
             (read-clickhouse (open-input-string v))))))
    ((cons 'Tuple from-subtypes)
     (match to
       ((cons 'list to-subtypes)
        (map (lambda (from-type to-type v)
               (clickhouse-convert from-type to-type v))
             from-subtypes
             to-subtypes
             (read-clickhouse (open-input-string v))))))
    ('DateTime
     (match to
       ('date (match v
                ((? string?) (string->date v "~Y-~m-~d ~H:~M:~S"))))))
    ('Date (match to
             ('date (match v
                      ((? string?) (string->date v "~Y-~m-~d"))))))))

(module+ test
  (require rackunit)

  (check-equal? (clickhouse-transition 'UInt8) 'number)
  (check-equal? (clickhouse-transition 'Float32) 'number)
  (check-equal? (clickhouse-transition 'String) 'string)
  (check-equal? (clickhouse-transition 'FixedString) 'string)
  (check-equal? (clickhouse-transition 'Date) 'date)
  (check-equal? (clickhouse-transition 'DateTime) 'date)
  (check-equal? (clickhouse-transition '(Array UInt32)) '(list number))
  (check-equal? (clickhouse-transition '(Tuple UInt32 UInt8)) '(list number number))

  (check-equal? (clickhouse-convert 'UInt8 'number "1") 1)
  (check-equal? (clickhouse-convert 'UInt8 'string "1") "1")
  ;;(check-equal? (clickhouse-convert 'UInt128 'string "111111111111111") "1")
  (check-equal? (clickhouse-convert 'Int8 'number "1") 1)
  (check-equal? (clickhouse-convert 'Float32 'number "1.5") 1.5)
  (check-equal? (clickhouse-convert 'String 'string "1") "1")
  (check-equal? (clickhouse-convert 'FixedString 'string "1") "1")

  (check-equal? (clickhouse-convert 'UInt32 'date "1536501059")
                (date* 59 50 13 9 9 2018 0 251 #f 0 0 "UTC"))
  (check-equal? (clickhouse-convert 'DateTime 'date "2018-10-09 15:39:10")
                (date* 10 39 15 9 10 2018 2 281 #f 0 0 ""))
  (check-equal? (clickhouse-convert 'Date 'date "2018-10-09")
                (date* 0 0 0 9 10 2018 2 281 #f 0 0 ""))
  (check-equal? (clickhouse-convert 'Date 'date "2018-10-09 15:39:10")
                (date* 0 0 0 9 10 2018 2 281 #f 0 0 ""))

  (check-equal? (clickhouse-convert '(Array String) '(list string) "('hello')")
                (list "hello"))
  (check-equal? (clickhouse-convert '(Array Date)
                                    '(list date)
                                    "('2018-10-09','2018-10-10 15:39:10','2018-10-11')")
                (list
                 (date* 0 0 0 9 10 2018 2 281 #f 0 0 "")
                 (date* 0 0 0 10 10 2018 3 282 #f 0 0 "")
                 (date* 0 0 0 11 10 2018 4 283 #f 0 0 "")))
  (check-equal? (clickhouse-convert '(Array DateTime)
                                    '(list date)
                                    "('2018-10-09 11:39:10','2018-10-10 12:40:11','2018-10-11 13:41:12')")
                (list
                 (date* 10 39 11 9 10 2018 2 281 #f 0 0 "")
                 (date* 11 40 12 10 10 2018 3 282 #f 0 0 "")
                 (date* 12 41 13 11 10 2018 4 283 #f 0 0 "")))
  (check-equal? (clickhouse-convert '(Array UInt32)
                                    '(list date)
                                    "(1536501059,1536501060,1536501061)")
                (list
                 (date* 59 50 13 9 9 2018 0 251 #f 0 0 "UTC")
                 (date* 0 51 13 9 9 2018 0 251 #f 0 0 "UTC")
                 (date* 1 51 13 9 9 2018 0 251 #f 0 0 "UTC")))
  (check-equal? (clickhouse-convert '(Tuple String String UInt8)
                                    '(list string string number)
                                    "('hello','you',2)")
                (list "hello" "you" 2))
  (check-equal? (clickhouse-convert '(Tuple Date DateTime String)
                                    '(list date date number)
                                    "('2018-10-09 11:39:10','2018-10-10 12:40:11','2')")
                (list
                 (date* 0 0 0 9 10 2018 2 281 #f 0 0 "")
                 (date* 11 40 12 10 10 2018 3 282 #f 0 0 "")
                 2)))
