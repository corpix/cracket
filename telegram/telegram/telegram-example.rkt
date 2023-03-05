#lang racket
(require "telegram.rkt")

(parameterize (;;(current-telegram-url "http://127.0.0.1:8888")
               (current-telegram-token "REDACTED"))
  (telegram-loop update
                 (match update
                   ((hash-table ("inline_query" inline-query) rest ...)
                    (displayln inline-query))
                   ((hash-table ("message" message) rest ...)
                    (match message
                      ((hash-table ("chat" chat) rest ...)
                       (match chat
                         ((hash-table ("id" chat-id) rest ...)
                          (with-input-from-file "/home/user/projects/src/git.backbone/corpix/racket/mpv-shot0001.jpg"
                            (thunk ;; (parameterize ((current-telegram-url "http://127.0.0.1:8888"))
                                     (displayln 'sendingfile)
                                     (telegram-send-photo #:chat-id chat-id
                                                          #:photo (current-input-port)))
                            );; )
                          )))))
                   (else
                    (displayln update)))))
