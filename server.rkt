#lang racket/base

(require 
  racket/string
  racket/path
  racket/file
  net/url-structs
  web-server/http
  web-server/templates
  web-server/dispatch
  web-server/servlet-env
  "common.rkt")

(define document-root
  (path->string (current-directory)))

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
    #:port 8386)
