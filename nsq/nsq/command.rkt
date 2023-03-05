#lang racket/base
(require racket/bytes
         racket/match
         corpix/json
         "frame.rkt"
         "io.rkt"
         "type.rkt")

(provide (all-defined-out))

(define (command->bytes cmd)
  (define name (string->bytes/utf-8
               (string-upcase (symbol->string (command-name cmd)))))

  ;; commands without body should have \n suffix
  ;; commands with body should not
  (match cmd
    ((command 'identify _ body)
     (let ((buf (command-body->bytes body)))
       (bytes-append
        name #"\n"
        (marshal-frame-size (bytes-length buf)) buf)))
    ((command 'pub (list topic) body)
     (let ((buf (command-body->bytes body)))
       (bytes-append
        name #" " (command-argument->bytes topic) #"\n"
        (marshal-frame-size (bytes-length buf)) buf)))
    ((command (or 'sub 'rdy 'fin) arguments _)
     (bytes-append
      name #" " (command-arguments->bytes arguments) #"\n"))
    ((command (or 'cls 'nop) _ _) (bytes-append name #"\n"))))

(define (command-arguments->bytes arguments)
  (bytes-join
   (map command-argument->bytes arguments)
   #" "))

(define (command-argument->bytes argument)
  (match argument
    ((? bytes?)   argument)
    ((? string?) (string->bytes/utf-8 argument))
    ((? symbol?) (command-argument->bytes (symbol->string argument)))
    ((? number?) (command-argument->bytes (number->string argument)))
    ((? hash?)   (json->bytes argument))))

(define command-body->bytes command-argument->bytes)

(module+ test
  (require rackunit)

  (test-case "command->bytes"
    (check-equal? (command->bytes (command 'identify #f #hasheq((foo . "bar"))))
                  #"IDENTIFY\n\0\0\0\r{\"foo\":\"bar\"}\n")
    (check-equal? (command->bytes (command 'pub '(#"topic") #"body"))
                  #"PUB topic\n\0\0\0\4body\n")
    (check-equal? (command->bytes (command 'sub '("topic" "channel") #f))
                  #"SUB topic channel\n")
    (check-equal? (command->bytes (command 'rdy '(666) #f)) #"RDY 666\n")
    (check-equal? (command->bytes (command 'fin '("d3adb33f") #f)) #"FIN d3adb33f\n")
    (check-equal? (command->bytes (command 'cls #f #f)) #"CLS\n")
    (check-equal? (command->bytes (command 'nop #f #f)) #"NOP\n"))

  (test-case "command-arguments->bytes"
    (check-equal? (command-arguments->bytes '(#"foo" "bar" baz 1 #hasheq((qux . 666))))
                  #"foo bar baz 1 {\"qux\":666}")))
