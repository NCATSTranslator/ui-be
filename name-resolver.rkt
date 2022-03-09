#lang racket/base

(require
  racket/match
  net/http-client
  json
  "common.rkt"

  racket/pretty)

(provide 
  lookup)

(define nr-host "name-resolution-sri.renci.org")
(define (nr-lookup-uri str offset limit)
  (format "/lookup?string=~a&offset=~a&limit=~a" str offset limit))

; For now get all matching CURIEs 
(define (parse-lookup resp)
  (jsexpr-object-keys resp))

(define (lookup str (offset 0) (limit 10))
  (match-define-values (_ _ resp-out)
    (http-sendrecv nr-host
                  (nr-lookup-uri str offset limit)
                  #:ssl? #t
                  #:method #"POST"
                  #:headers `(,head:acc/app/json)))
  (parse-lookup (read-json resp-out)))