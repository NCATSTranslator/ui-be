#lang racket/base

(provide (all-defined-out))

(define (empty-string? str)
  (= (string-length str) 0))
(define (string-add-prefix prefix str)
  (string-append prefix str))

(define header:acc/json (string->bytes/utf-8 "accept: application/json"))

(define mime:text (string->bytes/utf-8 "text/plain;charset=utf-8"))
(define mime:html (string->bytes/utf-8 "text/html;charset=utf-8"))
(define mime:json (string->bytes/utf-8 "application/json;charset=utf-8"))
(define mime:js   (string->bytes/utf-8 "text/javascript;charset=utf-8"))
(define mime:css  (string->bytes/utf-8 "text/css;charset=utf-8"))
(define mime:jpeg (string->bytes/utf-8 "image/jpeg"))
(define mime:svg  (string->bytes/utf-8 "image/svg+xml"))

(define (ext->mime-type ext)
  (case ext
    ((#"html") mime:html)
    ((#"js")   mime:js)
    ((#"css")  mime:css)
    ((#"jpeg") mime:jpeg)
    ((#"svg")  mime:svg)
    (else      mime:text)))

(define (jsexpr-object-keys je)
  (hash-keys je))
(define (jsexpr-object-values je)
  (hash-values je))
(define (jsexpr-object-has-key? je k)
  (hash-has-key? je k))
(define (jsexpr-object-ref je k)
  (hash-ref je k))
(define (jsexpr-object->alist je)
  (hash->list je))