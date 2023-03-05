#lang racket
(require "http.rkt")

(parameterize ((http-tls-security 'auto)) ;; INSECURE request
  (http "https://127.0.0.1:7788"))
