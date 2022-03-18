; TODO
; * /query
;   - What to do if ARS is down or times out?
; * /result

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
  json
  "common.rkt"
  "trapi.rkt"
  (prefix-in ars: "ars.rkt"))

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

(define (response/bad-request resp)
  (response
    400 #"Bad request"
    (current-seconds)
    mime:json
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

(define (response/internal-error resp)
  (response
    500 #"Internal server error"
    (current-seconds)
    mime:json
    '()
    (lambda (op)
      (write-bytes resp op))))

(define (failure reason)
  (jsexpr->bytes (hash 'reason reason)))

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
  (response/OK (string->bytes/utf-8 (include-template "build/index.html")) mime:html))

;X 1. Query cannot be converted to jsexpr (conversion to jsexpr)
;X 2. Query is not valid TRAPI (response from ARS)
;? 3. ARS is down (no response from ARS)
;X 4. ARS failed to post query (response from ARS)
(define (/query req)
  (with-handlers ((exn:fail:contract?
                  (lambda (e) (response/bad-request (failure "Query is not valid JSON")))))
    (define post-data (request-post-data/raw req))
    (define trapi-query (and post-data (qgraph->trapi-qgraph (bytes->jsexpr post-data))))
    (cond (trapi-query
            (define post-resp (ars:post-query trapi-query))
            (match (get-qcode (car post-resp))
              (200 (response/OK (string->bytes/utf-8 (cdr post-resp)) mime:text))
              (else (response/internal-error (failure "The ARS could not process the query"))))
          ((not post-data)
            (response/bad-request (failure "No query data")))
          (else
            (response/bad-request (failure "Query could not be converted to TRAPI"))))))

(define-values (dispatcher _)
  (dispatch-rules
    (("")      #:method "get"  /index)
    (("query") #:method "post" /query)
     (else                handle-static-file-request)))
        
(serve/servlet dispatcher 
    #:servlet-path ""
    #:servlet-regexp #rx""
    #:launch-browser? #f
    #:listen-ip #f
    #:port 8386)
