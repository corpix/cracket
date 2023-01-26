#lang racket

(provide parser-error
         parser-error?
         parser-error-state
         parser-error-buf
         parser-error-rule
         buffer-not-exhausted-error
         buffer-not-exhausted-error?
         buffer-not-exhausted-error-state
         buffer-not-exhausted-error-buf
         (struct-out parser-node)
         (struct-out parser-state)
         define-bnf)

(define-struct (parser-error exn)
  (state buf rule)
  #:constructor-name -make-parser-error)

(define make-parser-error*
  (case-lambda
    ((state buf rule)
     (make-parser-error*
      state buf rule
      (format "Failed to parse ~s with rule ~a at ~a."
	      (if buf (bytes->string/utf-8 buf) "")
	      'body
	      (parser-state-pos state))
      (current-continuation-marks)))
    ((state buf rule message cont)
     (-make-parser-error message cont state buf rule))))

(define-struct (buffer-not-exhausted-error exn)
  (state buf)
  #:constructor-name -make-buffer-not-exhausted-error)

(define make-buffer-not-exhausted-error*
  (case-lambda
    ((state buf)
     (make-buffer-not-exhausted-error*
      state buf
      (format "input buffer is not exhausted, last position: ~a, data left in buffer: ~s"
	      (parser-state-pos state)
	      (if buf (bytes->string/utf-8 buf) ""))
      (current-continuation-marks)))
    ((state buf message cont)
     (-make-buffer-not-exhausted-error message cont state buf))))

;;

(define-struct parser-node
  (name rule match child)
  #:transparent)

;;

(define-struct parser-state (buf pos depth)
  #:constructor-name -make-parser-state
  #:mutable)

(define (make-parser-state buf)
  (-make-parser-state
   (cond ((string? buf) (string->bytes/utf-8 buf))
         (else buf))
   0 0))

(define (parser-state-load! state state-copy)
  (set-parser-state-buf! state (parser-state-buf state-copy))
  (set-parser-state-pos! state (parser-state-pos state-copy))
  (set-parser-state-depth! state (parser-state-depth state-copy)))

(define (parser-state-match! state name rule do-match)
  (let ((buf (parser-state-buf state))
        (match (do-match state)))
    (and match
         (parser-node
          name rule
          (subbytes buf 0
                    (- (bytes-length buf)
                       (bytes-length (parser-state-buf state))))
          match))))

(define (parser-state-commit! state n)
  (begin0 (subbytes (parser-state-buf state) 0 n)
    (set-parser-state-buf!
     state
     (subbytes (parser-state-buf state)
               n (bytes-length (parser-state-buf state))))
    (set-parser-state-pos!
     state
     (+ (parser-state-pos state) n))))

;; ~bnf-rule-aux expands a rule and returns a lambda
;; which receives `state` and returns (vector ...) or #f.
;; it is ok for vector to be empty.
;; results should not be wrapped into `parser-node`.
(define-syntax (~bnf-rule-aux stx)
  (syntax-case stx (or and * + lambda)
    ((_ (or rule ...))
     ;; TODO: static rules
     (syntax/loc stx (lambda (state) (or ((~bnf-rule-aux rule) state) ...))))
    ((_ (and rule ...))
     (syntax/loc stx
       (let* ((matchers (list (~bnf-rule-aux rule) ...))
              (do-match (lambda (state)
                          (let loop ((acc (vector)) (ms matchers))
                            (if (pair? ms)
                                (let ((s (bytes->string/utf-8 (parser-state-buf state)))
                                      (match ((car ms) state)))
                                  (and match (loop
                                              (vector-append acc (vector match))
                                              (cdr ms))))
                                acc)))))
         (lambda (state)
           (let* ((state-copy (struct-copy parser-state state))
                  (match (do-match state-copy)))
             (begin0 match
               (and match (parser-state-load! state state-copy))))))))
    ((_ (* rule))
     (syntax/loc stx
       (let ((do-match (~bnf-rule-aux rule)))
         (lambda (state)
           (let loop ((acc (vector)))
             (let ((match (do-match state)))
               (if match
                   (loop (vector-append acc (vector match)))
                   acc)))))))
    ((_ (+ rule))
     (syntax/loc stx
       (let ((do-match (~bnf-rule-aux rule)))
         (lambda (state)
           (let loop ((acc (vector)) (n 0))
             (let ((match (do-match state)))
               (if match
                   (loop (vector-append acc (vector match)) (+ n 1))
                   (and (> n 0) acc))))))))
    ((_ (lambda (state) body ...))
     (syntax/loc stx (lambda (state) body ...)))

    ((_ rule)
     (let ((rule-val (syntax->datum (syntax rule))))
       (with-syntax
	   ((transformer
	     (cond
               ((string? rule-val)
                (syntax/loc stx (let ((r (string->bytes/utf-8 rule)))
                                  (lambda (state)
                                    (and (>= (bytes-length (parser-state-buf state))
                                             (bytes-length r))
                                         (let ((sub (subbytes (parser-state-buf state)
                                                                 0 (bytes-length r))))
                                           (and (equal? sub r)
                                                (parser-state-commit! state (bytes-length r))
                                                (vector))))))))
               ((symbol? rule-val) (syntax/loc stx rule))
               (else (error "Expected symbol or string, got" rule-val)))))
	 (syntax/loc stx transformer))))))

;; ~bnf-parse-aux expands all rules providen and
;; returns lambda which consumes an input to provide
;; the `parser-node` or raise an exception in case:
;; - there was no match in `input`
;; - input buffer is not drained (not all data was read)
(define-syntax (~bnf-parse-aux stx)
  (syntax-case stx ()
    ((_ ((var val) ...) body)
     (syntax/loc stx
       (let* ((var  (lambda (state)
                      (parser-state-match!
                       state 'var 'val
                       (~bnf-rule-aux val))))
              ...
              (proc (lambda (state) (parser-state-match!
                                     state #f 'body
                                     (~bnf-rule-aux body)))))
         (lambda (input)
           (let* ((state (make-parser-state input))
                  (child (proc state))
                  (buf   (and child (parser-state-buf state))))
             ;; TODO: this is an extension point, we probably want to have
             ;; an implementation which would not fail with exceptions
             ;; of such kind (non exhausting)
             (begin0 child
               (unless child
                 (raise (make-parser-error* state buf 'body)))
               (when (> (bytes-length buf) 0)
                 (raise (make-buffer-not-exhausted-error* state buf)))))))))))

;; define-bnf defines a parser with `name`
;; which is represented by a lambda
;; using `rules` and `body` for parsing
;; input passed to a lambda as first argument.
(define-syntax (define-bnf stx)
  (syntax-case stx ()
    ((_ name rules body)
     (syntax/loc stx (define name (~bnf-parse-aux rules body))))))


(module+ test
  (require rackunit)
  (test-case "terminals"
    (define-bnf terminals-test
      ((t "test")
       (y "you")
       (t/y (or t y)))
      t/y)

    (check-equal?
     (terminals-test "test")
     (parser-node
      #f 't/y (list->bytes '(116 101 115 116))
      (parser-node
       't/y '(or t y) (list->bytes '(116 101 115 116))
       (parser-node 't "test" (list->bytes '(116 101 115 116)) (vector)))))
    (check-equal?
     (terminals-test (list->bytes '(116 101 115 116)))
     (parser-node
      #f 't/y (list->bytes '(116 101 115 116))
      (parser-node
       't/y '(or t y) (list->bytes '(116 101 115 116))
       (parser-node 't "test" (list->bytes '(116 101 115 116)) (vector)))))
    (check-equal?
     (terminals-test "you")
     (parser-node
      #f 't/y (list->bytes '(121 111 117))
      (parser-node
       't/y '(or t y) (list->bytes '(121 111 117))
       (parser-node 'y "you" (list->bytes '(121 111 117)) (vector)))))
    (check-equal?
     (terminals-test (list->bytes '(121 111 117)))
     (parser-node
      #f 't/y (list->bytes '(121 111 117))
      (parser-node
       't/y '(or t y) (list->bytes '(121 111 117))
       (parser-node 'y "you" (list->bytes '(121 111 117)) (vector)))))

    (check-exn buffer-not-exhausted-error? (thunk (terminals-test "testyou")))
    (check-exn parser-error? (thunk (terminals-test "y")))
    (check-exn parser-error? (thunk (terminals-test ""))))
  (test-case "booleans"
    (define-bnf booleans-test
      ((t "test")
       (y "you")
       (ty (or t y))
       (ty&ty (and ty ty)))
      ty&ty)
    (check-equal?
     (booleans-test "testyou")
     (parser-node
      #f 'ty&ty (list->bytes '(116 101 115 116 121 111 117))
      (parser-node
       'ty&ty '(and ty ty) (list->bytes '(116 101 115 116 121 111 117))
       (vector
        (parser-node 'ty '(or t y) (list->bytes '(116 101 115 116))
                     (parser-node 't "test" (list->bytes '(116 101 115 116)) (vector)))
        (parser-node 'ty '(or t y) (list->bytes '(121 111 117))
                     (parser-node 'y "you" (list->bytes '(121 111 117)) (vector)))))))
    (check-equal?
     (booleans-test "youtest")
     (parser-node
      #f 'ty&ty (list->bytes '(121 111 117 116 101 115 116))
      (parser-node
       'ty&ty '(and ty ty) (list->bytes '(121 111 117 116 101 115 116))
       (vector
        (parser-node 'ty '(or t y) (list->bytes '(121 111 117))
                     (parser-node 'y "you" (list->bytes '(121 111 117)) (vector)))
        (parser-node 'ty '(or t y) (list->bytes '(116 101 115 116))
                     (parser-node 't "test" (list->bytes '(116 101 115 116)) (vector)))))))

    (check-equal?
     (booleans-test "testtest")
     (parser-node
      #f 'ty&ty (list->bytes '(116 101 115 116 116 101 115 116))
      (parser-node
       'ty&ty '(and ty ty) (list->bytes '(116 101 115 116 116 101 115 116))
       (vector
        (parser-node 'ty '(or t y) (list->bytes '(116 101 115 116))
                     (parser-node 't "test" (list->bytes '(116 101 115 116)) (vector)))
        (parser-node 'ty '(or t y) (list->bytes '(116 101 115 116))
                     (parser-node 't "test" (list->bytes '(116 101 115 116)) (vector)))))))
    (check-equal?
     (booleans-test "youyou")
     (parser-node
      #f 'ty&ty (list->bytes '(121 111 117 121 111 117))
      (parser-node
       'ty&ty '(and ty ty) (list->bytes '(121 111 117 121 111 117))
       (vector
        (parser-node 'ty '(or t y) (list->bytes '(121 111 117))
                     (parser-node 'y "you" (list->bytes '(121 111 117)) (vector)))
        (parser-node 'ty '(or t y) (list->bytes '(121 111 117))
                     (parser-node 'y "you" (list->bytes '(121 111 117)) (vector)))))))
    (check-exn buffer-not-exhausted-error? (thunk (booleans-test "testyoutest")))
    (check-exn parser-error? (thunk (booleans-test "y")))
    (check-exn parser-error? (thunk (booleans-test ""))))
  (test-case "repeat zero or more"
    (define-bnf repeat*-test
      ((t "test")
       (t* (* t)))
      t*)

    (check-equal?
     (repeat*-test "")
     (parser-node #f 't* #"" (parser-node 't* '(* t) #"" (vector))))
    (check-equal?
     (repeat*-test "test")
     (parser-node
      #f 't* (list->bytes '(116 101 115 116))
      (parser-node
       't* '(* t) (list->bytes '(116 101 115 116))
       (vector (parser-node 't "test" (list->bytes '(116 101 115 116)) (vector))))))
    (check-equal?
     (repeat*-test "testtest")
     (parser-node
      #f 't* (list->bytes '(116 101 115 116 116 101 115 116))
      (parser-node
       't* '(* t) (list->bytes '(116 101 115 116 116 101 115 116))
       (vector (parser-node 't "test" (list->bytes '(116 101 115 116)) (vector))
               (parser-node 't "test" (list->bytes '(116 101 115 116)) (vector))))))
    (check-equal?
     (repeat*-test "testtesttest")
     (parser-node
      #f 't* (list->bytes '(116 101 115 116 116 101 115 116 116 101 115 116))
      (parser-node
       't* '(* t) (list->bytes '(116 101 115 116 116 101 115 116 116 101 115 116))
       (vector (parser-node 't "test" (list->bytes '(116 101 115 116)) (vector))
               (parser-node 't "test" (list->bytes '(116 101 115 116)) (vector))
               (parser-node 't "test" (list->bytes '(116 101 115 116)) (vector)))))))
  (test-case "repeat one and more"
    (define-bnf repeat+-test
      ((t "test")
       (t+ (+ t)))
      t+)

    (check-equal?
     (repeat+-test "test")
     (parser-node
      #f 't+ (list->bytes '(116 101 115 116))
      (parser-node
       't+ '(+ t) (list->bytes '(116 101 115 116))
       (vector (parser-node 't "test" (list->bytes '(116 101 115 116)) (vector))))))
    (check-equal?
     (repeat+-test "testtest")
     (parser-node
      #f 't+ (list->bytes '(116 101 115 116 116 101 115 116))
      (parser-node
       't+ '(+ t) (list->bytes '(116 101 115 116 116 101 115 116))
       (vector (parser-node 't "test" (list->bytes '(116 101 115 116)) (vector))
               (parser-node 't "test" (list->bytes '(116 101 115 116)) (vector))))))
    (check-equal?
     (repeat+-test "testtesttest")
     (parser-node
      #f 't+ (list->bytes '(116 101 115 116 116 101 115 116 116 101 115 116))
      (parser-node
       't+ '(+ t) (list->bytes '(116 101 115 116 116 101 115 116 116 101 115 116))
       (vector (parser-node 't "test" (list->bytes '(116 101 115 116)) (vector))
               (parser-node 't "test" (list->bytes '(116 101 115 116)) (vector))
               (parser-node 't "test" (list->bytes '(116 101 115 116)) (vector))))))))
