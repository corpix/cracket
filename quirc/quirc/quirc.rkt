#lang racket
(require ffi/unsafe
         racket/generator
         racket/draw
         (for-syntax racket/syntax))
(provide exn:fail:quirc:decode
         exn:fail:quirc:not-found
         quirc-decode)
(module+ test
  (require rackunit))

(struct exn:fail:quirc:decode    exn:fail:user (kind))
(struct exn:fail:quirc:not-found exn:fail:user ())

(define (lightness-gray r g b)
  (exact-round (/ (+ (max r g b) (min r g b)) 2)))

(define (average-gray r g b)
  (exact-round (/ (+ r g b) 3)))

(define (luminosity-gray r g b)
  (exact-round (+ (* 0.21 r) (* 0.72 g) (* 0.07 b))))

(define (bitmap-pixels bitmap channels)
  (define dc (new bitmap-dc% (bitmap bitmap)))
  (define-values (w h) (send dc get-size))
  (define width (exact-floor w))
  (define height (exact-floor h))
  (define pixels (make-bytes (* channels width height)))
  (send dc get-argb-pixels 0 0 width height pixels)
  (values width height pixels))

(define (make-quirc image
                    #:channels (channels 4)
                    #:grayscale-method (grayscale luminosity-gray))
  (define-values (width height pixels)
    (bitmap-pixels (make-object bitmap% image) channels))
  (define quirc (quirc_new))
  (quirc_resize quirc width height)
  (let ((buffer (quirc_begin quirc width height)))
    (for ((i (in-range 0 (bytes-length pixels) channels))
          (j (in-range (* width height))))
      (ptr-set! buffer _uint8 j
                (grayscale
                 (bytes-ref pixels (+ i 1))
                 (bytes-ref pixels (+ i 2))
                 (bytes-ref pixels (+ i 3))))))
  (quirc_end quirc)
  quirc)

(define (quirc-decode image)
  (generator
   ()
   (define quirc (make-quirc image))
   (define qr-codes (quirc_count quirc))
   (if (= qr-codes 0)
       (yield (exn:fail:quirc:not-found
               "quirc not found: no qr codes found on the picture"
               (current-continuation-marks)))
       (for ((i (in-range 0 qr-codes)))
         (define-values (data err) (quirc_decode (quirc_extract quirc i)))
         (yield
          (cond
            ((eq? err 'success)
             (list->string
              (for/list
                  ((c (in-array (quirc-data-payload data) 0
                                (quirc-data-payload-len data))))
                (integer->char c))))
            (else (exn:fail:quirc:decode
                   (format "quirc decode error: ~a" err)
                   (current-continuation-marks)
                   err))))))
   (quirc_destroy quirc)))

(module+ test
  (test-case "decode"
    (let ((next (quirc-decode "qr/test-qrcode1.png")))
      (check-equal? (next) "http://ilnk.me/coupons")
      (check-equal? (next) (void)))))


(define quirc-ecc-level-m       0)
(define quirc-ecc-level-l       1)
(define quirc-ecc-level-h       2)
(define quirc-ecc-level-q       3)

(define quirc-data-type-numeric 1)
(define quirc-data-type-alpha   2)
(define quirc-data-type-byte    4)
(define quirc-data-type-kanji   8)

(define quirc-max-bitmap  3917)
(define quirc-max-payload 8896)

(define _quirc-decode-error-t
  (_enum '(success
           invalid-grid-size
           invalid-version
           format-ecc
           data-ecc
           unknown-data-type
           data-overflow
           data-underflow)))

(define _quirc _pointer) ;; TODO: markers
(define-cstruct _quirc-point ((x _int) (y _int)))

(define-cstruct _quirc-code ((corners     (_array _quirc-point 4))
                             (size         _int)
                             (cell-bitmap (_array _uint8 quirc-max-bitmap))))

(define-cstruct _quirc-data ((version     _int)
                             (ecc-level   _int)
                             (mask        _int)
                             (data-type   _int)
                             (payload    (_array _uint8 quirc-max-payload))
                             (payload-len _int)
                             (eci         _uint32)))

(define quirc-lib (ffi-lib "libquirc" '("1.0" #f)))

(define-syntax (quirc-ffi stx)
  (syntax-case stx ()
    ((_ fn s)
     (with-syntax ((id (format-id #'fn "~a" (syntax->datum #'fn))))
       #'(define id (get-ffi-obj fn quirc-lib s))))))

(quirc-ffi "quirc_new"      (_fun -> _quirc))
(quirc-ffi "quirc_destroy"  (_fun _quirc -> _void))
(quirc-ffi "quirc_version"  (_fun -> _string))
(quirc-ffi "quirc_begin"    (_fun _quirc (_ptr i _int) (_ptr i _int) -> (_ptr i _uint8)))
(quirc-ffi "quirc_end"      (_fun _quirc -> _void))
(quirc-ffi "quirc_resize"   (_fun _quirc _int _int -> _int))
(quirc-ffi "quirc_count"    (_fun _quirc -> _int))
(quirc-ffi "quirc_strerror" (_fun _quirc-decode-error-t -> _string))
(quirc-ffi "quirc_extract"  (_fun _quirc _int (code : (_ptr o _quirc-code)) -> _void -> code))
(quirc-ffi "quirc_decode"   (_fun (_ptr i _quirc-code)
                                  (data : (_ptr o _quirc-data)) ->
                                  (err : _quirc-decode-error-t) -> (values data err)))
