#lang racket/base

(require 
  racket/string
  racket/path
  racket/file
  net/url-structs
  web-server/http
  web-server/templates
  web-server/dispatch
  web-server/servlet-env)

(define document-root
  (path->string (current-directory)))

(define mime:text (string->bytes/utf-8 "text/plain;charset=utf-8"))
(define mime:html (string->bytes/utf-8 "text/html;charset=utf-8"))
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

(define (response/OK resp mime-type)
  (response
    200 #"OK"
    (current-seconds)
    mime-type
    '()
    (lambda (op)
      (write-bytes resp op))))

(define (response/not-found)
  (response/xexpr
    #:code    404
    #:message #"Not found"
      `(html
        (body
          (p "Not found")))))

(define (handle-static-file-request req)
  (define uri (request-uri req))
  (define resource (map path/param-path (url-path uri))) 
  (define f (string-append document-root
                              "/build/"
                              (string-join resource "/")))
  (cond ((file-exists? f)
          (define ext (filename-extension f))
          (define mime-type (ext->mime-type ext))
          (define data (file->bytes f))
          (response/OK data mime-type))
        (else (response/not-found))))

(define (/index req)
  (response/output
    (lambda (op) (display (include-template "build/index.html") op))))

(define-values (dispatcher _)
  (dispatch-rules
    (("")  #:method "get" /index)
     (else                handle-static-file-request)))
        
(serve/servlet dispatcher 
    #:servlet-path ""
    #:servlet-regexp #rx""
    #:launch-browser? #f
    #:listen-ip #f
    #:port 8384)
