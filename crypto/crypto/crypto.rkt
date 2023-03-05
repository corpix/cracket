#lang racket
(require file/sha1
         net/base64)
(provide hash->sri)
(module+ test
  (require rackunit))

(define (hash->sri hash (type 'sha256))
  (let ((bytes (case type
                 ((sha1 sha256 sha512)
                  (hex-string->bytes hash)))))
    (when (void? bytes)
      (error (format "unsupported hash type ~a" type)))
    (let ((size (bytes-length bytes)))
      (unless (case type
                ((sha1)   (= 20 size))
                ((sha256) (= 32 size))
                ((sha512) (= 64 size)))
        (error (format "invalid hash size ~a for hash type ~a" size type))))
    (string-append (~a type) "-" (bytes->string/latin-1 (base64-encode bytes "")))))

(module+ test
  (test-case "hash->sri"
    (check-equal? (hash->sri "d0e12121d24dc7f1aebbac048bae09fa1f6507b6" 'sha1)
                  "sha1-0OEhIdJNx/Guu6wEi64J+h9lB7Y=")
    (check-equal? (hash->sri "5543cd111cc6edb49759bbcbce8268bcfd6532f77d581f23f94a07c601a2b906" 'sha256)
                  "sha256-VUPNERzG7bSXWbvLzoJovP1lMvd9WB8j+UoHxgGiuQY=")
    (check-equal? (hash->sri "ccb24a0a0d769a81c599d6479df522d53ae3fa784ca60cd9f00e08e71efcf58bfba5e015dc0633bbc6aa115fe8747eadb7d89e40ea94613553cb6794ea58a4ff" 'sha512)
                  "sha512-zLJKCg12moHFmdZHnfUi1Trj+nhMpgzZ8A4I5x789Yv7peAV3AYzu8aqEV/odH6tt9ieQOqUYTVTy2eU6lik/w==")))
