#lang racket
(require net/url-string
         net/base64
         (only-in pkg/dirs-catalog create-dirs-catalog)
         (rename-in pkg/lib
                    (get-all-pkg-details-from-catalogs get-packages)
                    (current-pkg-catalogs current-package-catalogs))
         json)
(module+ test
  (require rackunit))

(define current-package-catalog-urls
  (make-parameter (list (format "https://download.racket-lang.org/releases/~a/catalog/" (version))
                        "https://pkgs.racket-lang.org"
                        "https://planet-compats.racket-lang.org")))
(define current-packages (make-parameter (make-hash)))
(define current-packages-ignore (make-parameter (set "racket")))
(define current-package-url-hashes (make-parameter (make-hash)))
(define current-catalog-fields
  (make-parameter '(name
                    description
                    dependencies
                    license
                    source
                    checksum
                    versions)))

(define current-catalog-hash-algorithm (make-parameter 'sha1))
(define current-nix-hash-algorithm (make-parameter 'sha256))

;;

(define (assocv key collection)
  (let ((found (assoc key collection)))
    (and found (cdr found))))

(define (command executable . arguments)
  (match-let (((list out in pid err control)
               (let ((executable-absolute-path (find-executable-path executable)))
                 (unless executable-absolute-path
                   (error (format "can not find absolute path for executable ~s in PATH" executable)))
                 (apply process* executable-absolute-path arguments))))
    (close-output-port in)
    (let* ((logger (current-logger))
           (buffered-out #f)
           (buffered-err #f)
           (io (list (thread (thunk (set! buffered-out (port->string out))))
                     (thread (thunk (set! buffered-err (port->string err)))))))
      (control 'wait)
      (dynamic-wind void
                    (thunk (for ((thread (in-list io))) (sync thread))
                           (let ((code (control 'exit-code)))
                             (unless (= code 0)
                               (error (format "shell command '~a' exited with ~a code, err: ~a"
                                              (append (list executable) arguments) code
                                              (string-trim buffered-err "\n"
                                                           #:left? #t
                                                           #:right? #t)))))
                           (string-trim buffered-out "\n"
                                        #:left? #t
                                        #:right? #t))
                    (thunk (close-input-port out)
                           (close-input-port err))))))

(define (nix32->sri hash)
  (command "nix" "--experimental-features" "nix-command"
           "hash" "to-sri"
           "--type" (symbol->string (current-nix-hash-algorithm))
           hash))

(define (git-hash-discover url rev)
  (log-info "obtaining ~a hash..." url)
  (let ((hash (or (hash-ref (current-package-url-hashes) url #f)
                  (nix32->sri (hash-ref (with-input-from-string
                                          (command "nix-prefetch-git" "--no-deepClone"
                                                   "--url" url
                                                   "--rev" rev)
                                          read-json)
                                        (current-nix-hash-algorithm))))))
    (begin0 hash
      (hash-set! (current-package-url-hashes) url hash))))

(define (zip-hash-discover url)
  (log-info "obtaining ~a hash..." url)
  (let ((hash (or (hash-ref (current-package-url-hashes) url #f)
                  (nix32->sri (command "nix-prefetch-url" "--unpack" url)))))
    (begin0 hash
      (hash-set! (current-package-url-hashes) url hash))))

;;

(define (package-source-url url)
  (string->url
   (cond
     ((regexp-match #rx"^(git|github|http|https)://github.com/" url)
      (match-let (((list user repo rev path)
                   (match url
                     ((regexp #rx"^github://github.com/([^/]*)/([^/]*)/([^/]*)(/([^/]*))?$"
                              (list _ user repo rev _ path))
                      (list user repo rev path))
                     ((regexp #rx"^[^:]*://github.com/([^/]*)/([^/]*)[.]git/?([?]path=([^#]*))?(#(.*))?$"
                              (list _ user repo _ path _ rev))
                      (list user repo rev path))
                     ((regexp #rx"^[^:]*://github.com/([^/]*)/([^/?]*)/?([?]path=([^#]*))?(#(.*))?$"
                              (list _ user repo _ path _ rev))
                      (list user repo rev path))
                     ((regexp #rx"^[^:]*://github.com/([^/]*)/([^/]*)[.]git/tree/([^?]*)([?]path=(.*))?$"
                              (list _ user repo rev _ path))
                      (list user repo rev path))
                     ((regexp #rx"^[^:]*://github.com/([^/]*)/([^/]*)/tree/([^?]*)([?]path=(.*))?$"
                              (list _ user repo rev _ path))
                      (list user repo rev path)))))
        (~a "https://github.com/" user "/" repo ".git"
            (if (and path (> (string-length path) 0)) (~a "?path=" path) "")
            (if rev (~a "#" rev) ""))))
     (else url))))

;;

(define (catalogs->packages (urls (current-package-catalog-urls)))
  (let ((filter-dependency (lambda (dependency)
                             (let loop ((result #t) (sub dependency))
                               (match sub
                                 ((list #:platform _ rest ...) #f)
                                 ((list _ rest ...) (loop result rest))
                                 ((list) result)
                                 ((? string? _) result)))))
        (transform-dependency (lambda (dependency)
                                (let loop ((acc (make-hash))
                                           (sub dependency))
                                  (match sub
                                    ((list #:platform platform rest ...)
                                     (unless (regexp? platform)
                                       (hash-set! acc 'platform (format "~a" platform)))
                                     (loop acc rest))
                                    ((list #:version version rest ...)
                                     (hash-set! acc 'version version)
                                     (loop acc rest))
                                    ((list (? string? name) rest ...)
                                     (hash-set! acc 'name name)
                                     (loop acc rest))
                                    ((? string? name)
                                     (hash-set! acc 'name name)
                                     acc)
                                    ((list) acc))))))
    (parameterize ((current-package-catalogs (map string->url urls)))
      (for/hash (((name package) (in-hash (get-packages))))
        (values name (for/hash (((key value) (in-hash package))
                                #:when (member key (current-catalog-fields)))
                       (values key (case key
                                     ((dependencies)
                                      (map transform-dependency
                                           (filter filter-dependency value)))
                                     (else value)))))))))

(define (package-dependencies package (packages (current-packages)))
  (let loop ((dependencies (hash-ref package 'dependencies null))
             (acc (make-hash)))
    (begin0 acc
      (for ((dependency (in-list dependencies)))
        (let ((dependency-name (hash-ref dependency 'name)))
          (unless (set-member? (current-packages-ignore) dependency-name)
            (unless (hash-ref acc dependency-name #f)
              (let ((dependency-details (hash-ref packages dependency-name #f)))
                (unless dependency-details
                  (error (format "package ~a was not found in current catalogs: ~a"
                                 dependency-name (current-package-catalog-urls))))
                (hash-set! acc dependency-name dependency-details)
                (loop (hash-ref dependency-details 'dependencies null) acc)))))))))

(define (package-unwind package (packages (current-packages)))
  (let ((dependencies (package-dependencies package)))
    (hash-set! dependencies (hash-ref package 'name) package)
    dependencies))

;;

(define-struct nix-with
  (sym body)
  #:prefab)

(define-struct nix-let
  (bindings body)
  #:prefab)

(define-struct nix-inherit
  (from syms)
  #:prefab)

(define-struct nix-rec
  (expr)
  #:prefab)

(define-struct nix-binding
  (identifier expr)
  #:prefab)

(define-struct nix-function
  (arguments body)
  #:prefab)

(define-struct nix-function-pattern-match
  (patterns)
  #:prefab)

(define-struct nix-function-call
  (name arguments)
  #:prefab)

(define-struct nix-import
  (expr)
  #:prefab)

(define-struct nix-interpolation
  (expr)
  #:prefab)

(define-struct nix-dot
  (parts)
  #:prefab)

(define-struct nix-path
  (parts)
  #:prefab)

(define-struct nix-literal
  (expr)
  #:prefab)

(define-struct nix-string
  (expr)
  #:prefab)

(define (nix-generate value
                      #:level (level 0)
                      #:sequence-part? (sequence-part? #f))
  (let* ((space " ")
         (new-line "\n")
         (repeat (lambda (v n)
                   (string-join (for/list ((_ (in-range 0 n))) v) "")))
         (indentation (repeat space 2))
         (indent (lambda (v n)
                   (string-join
                    (map (lambda (line) (string-append indentation line))
                         (string-split v new-line))
                    new-line)))
         (expression (cond
                       ((list? value) (string-append
                                       "[" new-line
                                       (indent (string-join
                                                (map (lambda (inner)
                                                       (nix-generate inner
                                                                     #:level (+ 1 level)
                                                                     #:sequence-part? #t))
                                                     value)
                                                new-line)
                                               (+ 1 level))
                                       new-line "]"))
                       ((hash? value) (string-append
                                       "{" new-line
                                       (indent (string-join (for/list (((key value) (in-hash value)))
                                                              (string-append (format "~a = ~a"
                                                                                     (nix-generate (format "~a" key))
                                                                                     (nix-generate value #:level (+ 1 level)))))
                                                            new-line)
                                               (+ 1 level))
                                       new-line "}"))
                       ((number? value) (number->string value))
                       ((string? value) (string-append "\"" (string-replace value "\"" "\\\"") "\""))
                       ((symbol? value) (symbol->string value))
                       ((boolean? value) (if value "true" "false"))
                       ((nix-with? value)
                        (string-append "with " (~a (nix-with-sym value)) ";" "\n"
                                       (indent (nix-generate (nix-with-body value)
                                                             #:level (+ 1 level)
                                                             #:sequence-part? #t)
                                               (+ 1 level))))
                       ((nix-let? value)
                        (string-append "let" "\n"
                                       (indent (string-join (map (lambda (binding) (nix-generate binding #:level (+ 1 level)))
                                                                 (nix-let-bindings value))
                                                            "\n")
                                               (+ 1 level))
                                       "\n" "in" "\n"
                                       (indent (nix-generate (nix-let-body value) #:level (+ 1 level))
                                               (+ 1 level))))
                       ((nix-inherit? value)
                        (let ((from (nix-inherit-from value)))
                          (string-append "inherit"
                                         (if from
                                             (string-append " (" (nix-generate from) ")")
                                             "")
                                         "\n"
                                         (indent (string-join (map ~a (nix-inherit-syms value)) "\n")
                                                 (+ 1 level)))))
                       ((nix-rec? value)
                        (string-append "rec " (nix-generate (nix-rec-expr value)
                                                            #:level level
                                                            #:sequence-part? #t)))
                       ((nix-binding? value)
                        (string-append (nix-generate (nix-binding-identifier value)) " = "
                                       (nix-generate (nix-binding-expr value))))
                       ((nix-function? value)
                        (string-append "("
                                       (string-join (map (lambda (argument) (string-append (nix-generate argument) ": "))
                                                         (nix-function-arguments value)) "")
                                       (nix-generate (nix-function-body value))
                                       ")"))
                       ((nix-function-pattern-match? value)
                        (string-append
                         "{"
                         (string-join (map (lambda (pattern) (cond
                                                               ((cons? pattern)
                                                                (string-append (nix-generate (car pattern))
                                                                               " ? "
                                                                               (nix-generate (cdr pattern))))
                                                               (else (nix-generate pattern))))
                                           (nix-function-pattern-match-patterns value))
                                      ", ")
                         "}"))
                       ((nix-function-call? value)
                        (string-append "(" (nix-generate (nix-function-call-name value))
                                       " "
                                       (string-join (map (lambda (argument) (nix-generate argument
                                                                                          #:level (+ 1 level)
                                                                                          #:sequence-part? #t))
                                                         (nix-function-call-arguments value))
                                                    " ")
                                       ")"))
                       ((nix-import? value)
                        (string-append "import " (nix-generate (nix-import-expr value))))
                       ((nix-interpolation? value)
                        (string-append "${" (nix-generate (nix-interpolation-expr value) #:level level) "}"))
                       ((nix-dot? value)
                        (string-join (map nix-generate (nix-dot-parts value)) "."))
                       ((nix-path? value)
                        (string-join (map (lambda (part) (nix-generate part
                                                                       #:level level
                                                                       #:sequence-part? #t))
                                          (nix-path-parts value))
                                     "/"))
                       ((nix-literal? value)
                        (~a (nix-literal-expr value)))
                       ((nix-string? value)
                        (let ((expr (nix-string-expr value)))
                          (cond
                            ((list? expr) (string-append "''" "\n"
                                                         (indent (string-join
                                                                  (map (lambda (value)
                                                                         (if (string? value)
                                                                             value
                                                                             (nix-generate value)))
                                                                       expr)
                                                                  "")
                                                                 (+ 1 level))
                                                         "\n"
                                                         "''"))
                            (else (string-append "\"" (nix-generate expr) "\"")))))
                       (else (error (format "unexpected value type ~a" value))))))
    (if (and (> level 0) (not sequence-part?))
        (string-append expression ";")
        expression)))

(define (package-source->nix package)
  (let* ((url (package-source-url (hash-ref package 'source)))
         (url-path-string (path->string (url->path url)))
         (url-string (url->string (make-url
                                   (url-scheme url)
                                   (url-user url)
                                   (url-host url)
                                   (url-port url)
                                   (url-path-absolute? url)
                                   (url-path url)
                                   (filter (lambda (element)
                                             (let ((key (car element)))
                                               (not (eq? key 'path))))
                                           (url-query url))
                                   (url-fragment url))))
         (kind (cond ((string-suffix? url-path-string ".zip") 'zip)
                     ((string-suffix? url-path-string ".git") 'git)))
         (downloader (case kind
                       ((zip) 'fetchzip)
                       ((git) 'fetchgit)
                       (else #f)))
         (downloader-path (assocv 'path (url-query url)))
         (checksum (hash-ref package 'checksum))
         (arguments (case kind
                      ((zip)
                       `((url . ,url-string)
                         (hash . ,(zip-hash-discover url-string))
                         (stripRoot . #f)))
                      ((git)
                       `((url . ,url-string)
                         (rev . ,checksum)
                         (hash . ,(git-hash-discover url-string checksum))))
                      (else null)))
         (nix-expr (if downloader
                       (nix-function-call downloader (list (make-hash arguments)))
                       (nix-literal url-path-string)))
         (nix-expr (if downloader-path
                       (nix-function-call
                        'builtins.toPath
                        (list (nix-string
                               (nix-path
                                `(,(nix-interpolation nix-expr)
                                  ,@(map nix-literal (string-split downloader-path "/")))))))
                       nix-expr)))
    nix-expr))

(define (package-sources->nix (packages (current-packages)))
  (nix-function
   (list (nix-function-pattern-match
          (list (cons 'pkgs
                      (nix-function-call (nix-import '<nixpkgs>)
                                         (list (make-hash)))))))
   (nix-with 'pkgs
             (for/hash (((name metadata) (in-hash packages)))
               (values name (package-source->nix metadata))))))

(define (package->nix package (packages (current-packages)))
  (let* ((init-packages-catalog
          (nix-function '(name)
                        (nix-string (list "cp -r "
                                          (nix-interpolation (nix-dot (list 'racketPackages (nix-interpolation 'name))))
                                          " "
                                          "$out/pkgs/"
                                          (nix-interpolation 'name) "\n"
                                          "find $out/pkgs/" (nix-interpolation 'name) " " "-type d | xargs chmod 755"))))
         (build (list "mkdir $out" "\n"
                      "export HOME=$out" "\n"
                      "mkdir $out/pkgs" "\n"
                      (nix-interpolation (nix-function-call 'concatMapStringsSep
                                                            (list "\\n"
                                                                  init-packages-catalog
                                                                  (nix-function-call 'attrNames '(racketPackages)))))
                      "\n"
                      "racket -l- pkg/dirs-catalog --link $out/catalog $out/pkgs" "\n"
                      "raco pkg config --set catalogs file://$out/catalog"))
         (install (list "raco pkg install --binary --batch --auto --user" " " (nix-interpolation 'name) "\n"
                        "mkdir $out/bin" "\n"
                        "ln -s " (nix-interpolation 'racket) "/bin/racket $out/bin/racket" "\n"
                        "wrapProgram $out/bin/racket --set PLTADDONDIR $out/.local/share/racket")))
    (nix-function
     (list (nix-function-pattern-match
            `((pkgs . ,(nix-function-call (nix-import '<nixpkgs>)
                                          (list (make-hash))))
              (racketPackages . ,(nix-function-call (package-sources->nix (package-unwind package packages))
                                                    (list (make-hash '((pkgs . pkgs)))))))))
     (nix-with 'pkgs
               (nix-with 'lib
                         (nix-function-call
                          'stdenv.mkDerivation
                          (list (nix-rec (make-hash `((name . ,(hash-ref package 'name))
                                                      (buildInputs . (racket findutils makeWrapper))
                                                      (unpackPhase . ":")
                                                      (buildPhase . ,(nix-string build))
                                                      (installPhase . ,(nix-string install))))))))))))

;;

(create-dirs-catalog "/home/user/projects/src/git.backbone/corpix/cracket/pkg"
                     '("/home/user/projects/src/git.backbone/corpix/cracket"))

(current-packages (catalogs->packages (cons "file:///home/user/projects/src/git.backbone/corpix/cracket/pkg"
                                            (current-package-catalog-urls))))

(define test-package (hash-ref (current-packages) "corpix-http"))
(displayln (nix-generate (package->nix test-package)))
