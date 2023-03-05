#lang racket/base
(provide (all-defined-out))

(define magic #"  V2")
(define parameters
  '(client_id hostname feature_negotiation tls_v1
              heartbeat_interval output_buffer_size output_buffer_timeout
              snappy deflate deflate_level
              sample_rate user_agent
              msg_timeout))
