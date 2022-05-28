; TODO
; * /query
;   - What to do if ARS is down or times out?
; * /result

#lang racket/base

(require 
  racket/string
  racket/path
  racket/file
  racket/match
  net/url-structs
  web-server/http
  web-server/templates
  web-server/dispatch
  web-server/servlet-env
  web-server/safety-limits
  json
  "common.rkt"
  (prefix-in trapi: "trapi.rkt")
  (prefix-in ars:   "ars.rkt")
  (prefix-in mock:  "mock/ars.rkt")
  (prefix-in mock:  "mock/trapi.rkt")
  "config.rkt"
  
  racket/pretty)

; Expose mockable procedures based on config
(define-values (post-query pull-query-status pull-query-result)
  (if (config-mock-ars? SERVER-CONFIG)
      (values mock:post-query mock:pull-query-status mock:pull-query-result)
      (values ars:post-query ars:pull-query-status ars:pull-query-result)))
(define qgraph->trapi-query
  (if (config-mock-query? SERVER-CONFIG)
      mock:qgraph->trapi-query
      trapi:qgraph->trapi-query))

(define (get-qid req-data)
  (jsexpr-object-ref req-data 'qid))

(define document-root (config-document-root SERVER-CONFIG))

(define (response/jsexpr code message jse)
  (response
    code
    message
    (current-seconds)
    mime:json
    '()
    (lambda (op)
      (write-bytes (jsexpr->bytes jse) op))))

(define (response/OK resp mime-type)
  (response
    200 #"OK"
    (current-seconds)
    mime-type
    '()
    (lambda (op)
      (write-bytes resp op))))

(define (response/OK/jsexpr jse)
  (response/jsexpr 200 #"OK" jse))

(define (response/bad-request jse)
  (response/jsexpr 400 #"Bad request" jse))

(define (response/not-found)
  (response/xexpr
    #:code    404
    #:message #"Not found"
      `(html
        (body
          (p "Not found")))))

(define (response/internal-error jse)
  (response/jsexpr 500 #"Internal server error" jse))

(define (make-response status (data '()))
  (hash 'status status
        'data   data))

(define (failure-response reason)
  (make-response "error" reason))

(define (file->response f)
  (cond ((file-exists? f)
          (define ext (filename-extension f))
          (define mime-type (ext->mime-type ext))
          (define data (file->bytes f))
          (response/OK data mime-type))
        (else (/index))))

(define (handle-static-file-request req)
  (define uri (request-uri req))
  (define resource (map path/param-path (url-path uri))) 
  (define f (string-append document-root "build/" (string-join resource "/")))
  (file->response f))

(define (/index (req #f))
  (define index.html (string-append document-root "build/index.html"))
  (file->response index.html))

;TODO: ARS is down (no response from ARS)
(define (/query req)
  (with-handlers ((exn:fail:contract?
                  (lambda (e) (response/bad-request (failure-response "Query is not valid JSON")))))
    (define post-data (request-post-data/raw req))
    (define trapi-query (and post-data (qgraph->trapi-query (bytes->jsexpr post-data))))
    (cond (trapi-query
            (define post-resp (post-query trapi-query))
            (pretty-print post-resp)
            (match (car post-resp)
              ('error (response/internal-error (failure-response "The ARS could not process the query")))
              (_  (response/OK/jsexpr (make-response "success" (cdr post-resp))))))
          ((not post-data)
            (response/bad-request (failure-response "No query data")))
          (else
            (response/bad-request (failure-response "Query could not be converted to TRAPI"))))))

(define (/result req)
  (with-handlers ((exn:fail:contract?
                  (lambda (e) (raise e))));(response/bad-request
                                ;(failure-response "Result poll does not contain a 'qid' attribute")))))
    (define post-data (request-post-data/raw req))
    (define qid (and post-data (get-qid (bytes->jsexpr post-data))))
    (define qstatus (pull-query-status qid))
    (pretty-print qid)
    (pretty-print qstatus)
    (match qstatus
      ('done
        (let ((result (pull-query-result qid)))
          (response/OK/jsexpr (trapi:add-summary (make-response "done" result)))))
      ('running
        (response/OK/jsexpr (make-response "running")))
      (_
        (response/internal-error (failure-response "Something went wrong"))))))
        

(define-values (dispatcher _)
  (dispatch-rules
    (("")       #:method "get"  /index)
    (("query")  #:method "post" /query)
    (("result") #:method "post" /result)
     (else                handle-static-file-request)))
        
(serve/servlet dispatcher 
    #:servlet-path ""
    #:servlet-regexp #rx""
    #:launch-browser? #f
    #:listen-ip #f
    #:port (config-port SERVER-CONFIG) 
    #:safety-limits (make-safety-limits 
                      #:response-timeout (config-response-timeout SERVER-CONFIG)))
