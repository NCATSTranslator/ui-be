#lang racket/base

(provide
  current-log-port
  log-request)

(require
  racket/pretty
  net/url-structs
  web-server/http/request-structs)

; access log - verbose mode logs params
; IP of request
; date/time
; type of request (GET POST)
; endpoint
; HTTP version
; return code
; bytes transferred
; time to serve request
; uuids
; exceptions go to error log

(define (pretty-timestamp)
  (define seconds (current-seconds))
  (define d       (seconds->date seconds #f))
  (list seconds 'UTC
        (date-year d) (date-month  d) (date-day    d)
        (date-hour d) (date-minute d) (date-second d)))

(define (request->log req)
  (define (uri->log uri)
    (define (path->log path)
      `((path  . ,(path/param-path path))
        (param . ,(path/param-param path))))

    `((scheme . ,(url-scheme uri))
      (host   . ,(url-host   uri))
      (port   . ,(url-port   uri))
      (path   . ,(path/param-path (car (url-path uri))))
      (query  . ,(url-query  uri))))

  (define (header->log header)
    `((field . ,(header-field header))
      (value . ,(header-value header))))

  `(request
     (client-ip . ,(request-client-ip req))
     (method    . ,(request-method req))
     (uri       . ,(uri->log (request-uri req)))
     (headers   . ,(map header->log (request-headers/raw req)))
     (data      . ,(request-post-data/raw req))))

(define current-log-port (make-parameter (current-error-port)))

(define (log-request req)
  (pretty-write (cons (pretty-timestamp) (request->log req)) (current-log-port))
  (flush-output (current-log-port)))
