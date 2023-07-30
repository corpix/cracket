#lang racket
(require corpix/list)
(provide struct-id)

(define (struct-id struct)
  (let ((type (car (values->list (struct-info struct)))))
    (car (values->list (struct-type-info type)))))
