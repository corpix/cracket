#lang racket
(require corpix/list)
(provide (all-defined-out))

(define (struct-id struct)
  (let ((type (car (values->list (struct-info struct)))))
    (car (values->list (struct-type-info type)))))

(define (struct-data struct)
  (cdr (vector->list (struct->vector struct))))
