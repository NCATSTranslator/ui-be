#lang racket/base

(provide
  current-log-port
  pretty-log)

(require
  net/url-structs
  web-server/http/request-structs)

(define current-log-port (make-parameter (current-error-port)))

(define (pretty-timestamp)
  (define seconds (current-seconds))
  (define d       (seconds->date seconds #f))
  (list seconds 'UTC
        (date-year d) (date-month  d) (date-day    d)
        (date-hour d) (date-minute d) (date-second d)))

(define (pretty-log . args)
  (apply writeln (list (cons (pretty-timestamp) args) (current-log-port))))
