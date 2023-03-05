#lang racket/base
(require racket/string
         racket/set
         racket/port
         racket/function
         racket/path
         file/glob
         json
         "tool.rkt"
         "type.rkt"
         "parameter.rkt")
(provide make-package
         package-info
         package-install
         package-installed?)

(define (make-package name)
  (package name (seteq)))

(define (package-files path)
  (map (compose path->string file-name-from-path)
       (glob (build-path path "*" (*extension*)))))

(define (package-info path)
  (let ((info (string->jsexpr
               (execute "go" "list" "-json"
                        (*->string path)))))
    (hash-set info 'GoLispFiles
              (package-files (hash-ref info 'Dir)))))

(define (package-install path)
  (let ((path-string (*->string path)))
    (execute "go" "get" path-string)
    (package-info path-string)))

(define (package-installed? path)
  ;; FIXME: confined package-info error type for «no such package» case
  (with-handlers ((exn? (lambda _ #f)))
    (and (package-info path)
         #t)))

;;(package-install 'github.com/pkg/errors)
;;(package-info 'github.com/pkg/errors)
