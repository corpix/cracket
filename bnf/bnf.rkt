#lang racket

(provide (except-out (struct-out bnf-exn)
                     -make-bnf-exn)
         (except-out (struct-out bnf-parser-not-exhausted-exn)
                     -make-bnf-parser-not-exhausted-exn)
         (struct-out bnf-node)
         (except-out (struct-out bnf-state)
                     -make-bnf-state)
         make-bnf-state
         define-bnf)

(define-struct (bnf-exn exn)
  (state buf rule)
  #:constructor-name -make-bnf-exn)

(define make-bnf-exn*
  (case-lambda
    ((state buf rule)
     (make-bnf-exn*
      state buf rule
      (format "Failed to parse ~s with rule ~a at ~a."
	      (if buf (bytes->string/utf-8 buf) "")
	      'body
	      (bnf-state-pos state))
      (current-continuation-marks)))
    ((state buf rule message cont)
     (-make-bnf-exn message cont state buf rule))))

(define-struct (bnf-parser-not-exhausted-exn exn)
  (state buf)
  #:constructor-name -make-bnf-parser-not-exhausted-exn)

(define make-bnf-parser-not-exhausted-exn*
  (case-lambda
    ((state buf)
     (make-bnf-parser-not-exhausted-exn*
      state buf
      (format "input buffer is not exhausted, last position: ~a, data left in buffer: ~s"
	      (bnf-state-pos state)
	      (if buf (bytes->string/utf-8 buf) ""))
      (current-continuation-marks)))
    ((state buf message cont)
     (-make-bnf-parser-not-exhausted-exn message cont state buf))))

;;

(define-struct bnf-node
  (name rule match child)
  #:transparent)

;;

(define-struct bnf-state (buf pos depth)
  #:constructor-name -make-bnf-state
  #:mutable)

(define (make-bnf-state buf)
  (-make-bnf-state
   (cond ((string? buf) (string->bytes/utf-8 buf))
         (else buf))
   0 0))

(define (bnf-state-load! state state-copy)
  (set-bnf-state-buf! state (bnf-state-buf state-copy))
  (set-bnf-state-pos! state (bnf-state-pos state-copy))
  (set-bnf-state-depth! state (bnf-state-depth state-copy)))

(define (bnf-state-match! state name rule do-match)
  (let ((buf (bnf-state-buf state))
        (match (do-match state)))
    (and match
         (bnf-node
          name rule
          (subbytes buf 0
                    (- (bytes-length buf)
                       (bytes-length (bnf-state-buf state))))
          match))))

(define (bnf-state-commit! state n)
  (begin0 (subbytes (bnf-state-buf state) 0 n)
    (set-bnf-state-buf!
     state
     (subbytes (bnf-state-buf state)
               n (bytes-length (bnf-state-buf state))))
    (set-bnf-state-pos!
     state
     (+ (bnf-state-pos state) n))))

;; ~bnf-rule-aux expands a rule and returns a lambda
;; which receives `state` and returns (vector ...) or #f.
;; it is ok for vector to be empty.
;; results should not be wrapped into `bnf-node`.
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
                                (let ((s (bytes->string/utf-8 (bnf-state-buf state)))
                                      (match ((car ms) state)))
                                  (and match (loop
                                              (vector-append acc (vector match))
                                              (cdr ms))))
                                acc)))))
         (lambda (state)
           (let* ((state-copy (struct-copy bnf-state state))
                  (match (do-match state-copy)))
             (begin0 match
               (and match (bnf-state-load! state state-copy))))))))
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
                                    (and (>= (bytes-length (bnf-state-buf state))
                                             (bytes-length r))
                                         (let ((sub (subbytes (bnf-state-buf state)
                                                                 0 (bytes-length r))))
                                           (and (equal? sub r)
                                                (bnf-state-commit! state (bytes-length r))
                                                (vector))))))))
               ((symbol? rule-val) (syntax/loc stx rule))
               (else (error "expected symbol or string, got" rule-val)))))
	 (syntax/loc stx transformer))))))

;; ~bnf-parse-aux expands all rules providen and
;; returns lambda which consumes an input to provide
;; the `bnf-node` or raise an exception in case:
;; - there was no match in `input`
;; - input buffer is not drained (not all data was read)
(define-syntax (~bnf-parse-aux stx)
  (syntax-case stx ()
    ((_ ((var val) ...) body)
     (syntax/loc stx
       (let* ((var  (lambda (state)
                      (bnf-state-match! state 'var 'val
                                        (~bnf-rule-aux val))))
              ...
              (proc (lambda (state) (bnf-state-match! state #f 'body
                                                      (~bnf-rule-aux body)))))
         (lambda (input)
           (let* ((state (make-bnf-state input))
                  (child (proc state))
                  (buf   (and child (bnf-state-buf state))))
             ;; TODO: this is an extension point, we probably want to have
             ;; an implementation which would not fail with exceptions
             ;; of such kind (non exhausting)
             (begin0 child
               (unless child
                 (raise (make-bnf-exn* state buf 'body)))
               (when (> (bytes-length buf) 0)
                 (raise (make-bnf-parser-not-exhausted-exn* state buf)))))))))))

;; let-bnf is like let, but for bnf rules.
(define-syntax (let-bnf stx)
  (syntax-case stx ()
    ((_ rules body)
     (syntax/loc stx (~bnf-parse-aux rules body)))))

;; define-bnf defines a bnf with `name`
;; which is represented by a lambda
;; using `rules` and `body` for parsing
;; input passed to a lambda as first argument.
(define-syntax (define-bnf stx)
  (syntax-case stx ()
    ((_ name rules body)
     (syntax/loc stx (define name (let-bnf rules body))))))



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
     (bnf-node
      #f 't/y (list->bytes '(116 101 115 116))
      (bnf-node
       't/y '(or t y) (list->bytes '(116 101 115 116))
       (bnf-node 't "test" (list->bytes '(116 101 115 116)) (vector)))))
    (check-equal?
     (terminals-test (list->bytes '(116 101 115 116)))
     (bnf-node
      #f 't/y (list->bytes '(116 101 115 116))
      (bnf-node
       't/y '(or t y) (list->bytes '(116 101 115 116))
       (bnf-node 't "test" (list->bytes '(116 101 115 116)) (vector)))))
    (check-equal?
     (terminals-test "you")
     (bnf-node
      #f 't/y (list->bytes '(121 111 117))
      (bnf-node
       't/y '(or t y) (list->bytes '(121 111 117))
       (bnf-node 'y "you" (list->bytes '(121 111 117)) (vector)))))
    (check-equal?
     (terminals-test (list->bytes '(121 111 117)))
     (bnf-node
      #f 't/y (list->bytes '(121 111 117))
      (bnf-node
       't/y '(or t y) (list->bytes '(121 111 117))
       (bnf-node 'y "you" (list->bytes '(121 111 117)) (vector)))))

    (check-exn bnf-parser-not-exhausted-exn? (thunk (terminals-test "testyou")))
    (check-exn bnf-exn? (thunk (terminals-test "y")))
    (check-exn bnf-exn? (thunk (terminals-test ""))))
  (test-case "booleans"
    (define-bnf booleans-test
      ((t "test")
       (y "you")
       (ty (or t y))
       (ty&ty (and ty ty)))
      ty&ty)
    (check-equal?
     (booleans-test "testyou")
     (bnf-node
      #f 'ty&ty (list->bytes '(116 101 115 116 121 111 117))
      (bnf-node
       'ty&ty '(and ty ty) (list->bytes '(116 101 115 116 121 111 117))
       (vector
        (bnf-node 'ty '(or t y) (list->bytes '(116 101 115 116))
                     (bnf-node 't "test" (list->bytes '(116 101 115 116)) (vector)))
        (bnf-node 'ty '(or t y) (list->bytes '(121 111 117))
                     (bnf-node 'y "you" (list->bytes '(121 111 117)) (vector)))))))
    (check-equal?
     (booleans-test "youtest")
     (bnf-node
      #f 'ty&ty (list->bytes '(121 111 117 116 101 115 116))
      (bnf-node
       'ty&ty '(and ty ty) (list->bytes '(121 111 117 116 101 115 116))
       (vector
        (bnf-node 'ty '(or t y) (list->bytes '(121 111 117))
                     (bnf-node 'y "you" (list->bytes '(121 111 117)) (vector)))
        (bnf-node 'ty '(or t y) (list->bytes '(116 101 115 116))
                     (bnf-node 't "test" (list->bytes '(116 101 115 116)) (vector)))))))

    (check-equal?
     (booleans-test "testtest")
     (bnf-node
      #f 'ty&ty (list->bytes '(116 101 115 116 116 101 115 116))
      (bnf-node
       'ty&ty '(and ty ty) (list->bytes '(116 101 115 116 116 101 115 116))
       (vector
        (bnf-node 'ty '(or t y) (list->bytes '(116 101 115 116))
                     (bnf-node 't "test" (list->bytes '(116 101 115 116)) (vector)))
        (bnf-node 'ty '(or t y) (list->bytes '(116 101 115 116))
                     (bnf-node 't "test" (list->bytes '(116 101 115 116)) (vector)))))))
    (check-equal?
     (booleans-test "youyou")
     (bnf-node
      #f 'ty&ty (list->bytes '(121 111 117 121 111 117))
      (bnf-node
       'ty&ty '(and ty ty) (list->bytes '(121 111 117 121 111 117))
       (vector
        (bnf-node 'ty '(or t y) (list->bytes '(121 111 117))
                     (bnf-node 'y "you" (list->bytes '(121 111 117)) (vector)))
        (bnf-node 'ty '(or t y) (list->bytes '(121 111 117))
                     (bnf-node 'y "you" (list->bytes '(121 111 117)) (vector)))))))
    (check-exn bnf-parser-not-exhausted-exn? (thunk (booleans-test "testyoutest")))
    (check-exn bnf-exn? (thunk (booleans-test "y")))
    (check-exn bnf-exn? (thunk (booleans-test ""))))
  (test-case "repeat zero or more"
    (define-bnf repeat*-test
      ((t "test")
       (t* (* t)))
      t*)

    (check-equal?
     (repeat*-test "")
     (bnf-node #f 't* #"" (bnf-node 't* '(* t) #"" (vector))))
    (check-equal?
     (repeat*-test "test")
     (bnf-node
      #f 't* (list->bytes '(116 101 115 116))
      (bnf-node
       't* '(* t) (list->bytes '(116 101 115 116))
       (vector (bnf-node 't "test" (list->bytes '(116 101 115 116)) (vector))))))
    (check-equal?
     (repeat*-test "testtest")
     (bnf-node
      #f 't* (list->bytes '(116 101 115 116 116 101 115 116))
      (bnf-node
       't* '(* t) (list->bytes '(116 101 115 116 116 101 115 116))
       (vector (bnf-node 't "test" (list->bytes '(116 101 115 116)) (vector))
               (bnf-node 't "test" (list->bytes '(116 101 115 116)) (vector))))))
    (check-equal?
     (repeat*-test "testtesttest")
     (bnf-node
      #f 't* (list->bytes '(116 101 115 116 116 101 115 116 116 101 115 116))
      (bnf-node
       't* '(* t) (list->bytes '(116 101 115 116 116 101 115 116 116 101 115 116))
       (vector (bnf-node 't "test" (list->bytes '(116 101 115 116)) (vector))
               (bnf-node 't "test" (list->bytes '(116 101 115 116)) (vector))
               (bnf-node 't "test" (list->bytes '(116 101 115 116)) (vector)))))))
  (test-case "repeat one and more"
    (define-bnf repeat+-test
      ((t "test")
       (t+ (+ t)))
      t+)

    (check-equal?
     (repeat+-test "test")
     (bnf-node
      #f 't+ (list->bytes '(116 101 115 116))
      (bnf-node
       't+ '(+ t) (list->bytes '(116 101 115 116))
       (vector (bnf-node 't "test" (list->bytes '(116 101 115 116)) (vector))))))
    (check-equal?
     (repeat+-test "testtest")
     (bnf-node
      #f 't+ (list->bytes '(116 101 115 116 116 101 115 116))
      (bnf-node
       't+ '(+ t) (list->bytes '(116 101 115 116 116 101 115 116))
       (vector (bnf-node 't "test" (list->bytes '(116 101 115 116)) (vector))
               (bnf-node 't "test" (list->bytes '(116 101 115 116)) (vector))))))
    (check-equal?
     (repeat+-test "testtesttest")
     (bnf-node
      #f 't+ (list->bytes '(116 101 115 116 116 101 115 116 116 101 115 116))
      (bnf-node
       't+ '(+ t) (list->bytes '(116 101 115 116 116 101 115 116 116 101 115 116))
       (vector (bnf-node 't "test" (list->bytes '(116 101 115 116)) (vector))
               (bnf-node 't "test" (list->bytes '(116 101 115 116)) (vector))
               (bnf-node 't "test" (list->bytes '(116 101 115 116)) (vector))))))))
