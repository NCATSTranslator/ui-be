#lang racket/base

(provide (all-defined-out))

(require
  racket/string
  racket/match
  xml/path)

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

(define (make-url-params params)
  (define (make-url-param param separator)
    (if param
        (format "~a~a" separator
                       (if (pair? param)
                           (string-append (car param) "=" (cdr param))
                           param))
        ""))

  (format "~a~a"
          (make-url-param (car params) "?")
          (apply string-append
                 (map (lambda (p) (make-url-param p "&"))
                      (cdr params)))))

(define (jsexpr-string? je)
  (string? je))
(define (jsexpr-object)
  (hash))
(define (make-jsexpr-object kvps)
  (make-immutable-hash kvps))
(define (jsexpr-object-keys je)
  (hash-keys je))
(define (jsexpr-object-values je)
  (hash-values je))
(define (jsexpr-object-has-key? je k)
  (hash-has-key? je k))
(define (jsexpr-object-ref je k (default #f))
  (hash-ref je k default))
(define (jsexpr-object-ref-recursive je ks (default #f))
  (let loop ((ks ks)
             (v je))
    (if (or (equal? v default) (null? ks))
        v
        (loop (cdr ks)
              (jsexpr-object-ref v (car ks) default)))))
(define (jsexpr-object-remove je k)
  (hash-remove je k))
(define (jsexpr-object-set je k v)
  (hash-set je k v))
(define (jsexpr-object-multi-set je kvps)
  (foldl (match-lambda**
           ((`(,key . ,val) (? jsexpr-object? je))
            (jsexpr-object-set je key val)))
         je
         kvps))
(define (jsexpr-object-transform je k proc (default #f))
  (jsexpr-object-set je k (proc (jsexpr-object-ref je k default))))
(define (jsexpr-object-key-map je keys proc (default #f))
  (let loop ((keys keys)
             (je je))
    (cond ((null? keys)
            je)
          (else
            (loop
              (cdr keys)
              (jsexpr-object-transform
                je
                (car keys)
                proc
                default))))))
(define (jsexpr-object-map je proc (default #f))
  (jsexpr-object-key-map je (jsexpr-object-keys je) proc default))

(define (jsexpr-object->alist je)
  (hash->list je))
(define (jsexpr-object-count je)
  (hash-count je))
(define (jsexpr-object? je)
  (hash? je))
(define (jsexpr-array)
  '())
(define (jsexpr-array? je)
  (list? je))
(define (jsexpr-array-prepend je e)
  (cons e je))
(define (jsexpr-null? je)
  (equal? je 'null))
(define (jsexpr-object-set-recursive jse ks v)
  (let loop ((ks ks)
             (o jse))
    (cond ((null? (cdr ks))
            (jsexpr-object-set o (car ks) v))
          (else
            (define k (car ks))
            (jsexpr-object-set o k
              (loop (cdr ks)
                    (jsexpr-object-ref o k (hash))))))))
(define (jsexpr-map proc je)
  (map proc je))

(define (tag->xexpr-value xexpr tag)
  (se-path* `(,tag) xexpr))
(define (tag->xexpr-fragments xexpr tag)
  (se-path*/list `(,tag) xexpr))
(define (tag->xexpr-subtree xexpr tag)
  `(,tag ,@(tag->xexpr-fragments xexpr tag)))

(define (yaml-ref ye k (default #f))
  (hash-ref ye (symbol->string k) default))

(define (biolink-tag str) (string-add-prefix "biolink:" str))
(define (make-biolink-tags strs) (map biolink-tag strs))
(define (strip-id-tag id) (cadr (string-split id ":")))
(define (host endpoint) (yaml-ref endpoint 'host))
(define (uri endpoint)  (yaml-ref endpoint 'uri))
(define (make-answer actor-data agent) (cons actor-data agent))
(define (answer-message answer) (car answer))
(define (answer-agent answer) (cdr answer))
(define (make-query-state status data) (cons status data))
(define (query-state-status query-state) (car query-state))
(define (query-state-data query-state) (cdr query-state))
