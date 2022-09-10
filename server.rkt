#lang racket/base

(require
  racket/pretty
  racket/string
  racket/path
  racket/file
  racket/port
  racket/match
  racket/function
  net/url-structs
  web-server/http
  web-server/templates
  web-server/dispatch
  web-server/servlet-env
  web-server/safety-limits
  json
  "common.rkt"
  "evidence.rkt"
  (prefix-in trapi: "trapi.rkt")
  (prefix-in ars:   "ars.rkt")
  (prefix-in log:   "logging.rkt")
  (prefix-in mock:  "mock/ars.rkt")
  (prefix-in mock:  "mock/trapi.rkt")
  (prefix-in mock:  "mock/evidence.rkt")
  "config.rkt")

; Expose mockable procedures based on config
(define-values (post-query pull-query-status pull-query-answers)
  (if (config-mock-ars? SERVER-CONFIG)
      (values mock:post-query mock:pull-query-status mock:pull-query-answers)
      (values ars:post-query ars:pull-query-status ars:pull-query-answers)))
(define-values (qgraph->trapi-query disease->creative-query)
  (if (config-mock-query? SERVER-CONFIG)
      (values mock:qgraph->trapi-query  mock:disease->creative-query)
      (values trapi:qgraph->trapi-query trapi:disease->creative-query)))
(define evidence-expanders
  (list (if (config-mock-pmid? SERVER-CONFIG)
            (mock:make-pmid-expander)
            (make-pmid-expander))
        (if (config-mock-nct? SERVER-CONFIG)
            (mock:make-nct-expander)
            (make-nct-expander))))

(define (get-req-qid req-data)
  (jsexpr-object-ref req-data 'qid))

(define (get-resp-qid resp-data)
  (define data (jsexpr-object-ref resp-data 'data))
  (cond ((string? data) data)
        ((jsexpr-object? data)
         (let ((qid (jsexpr-object-ref data 'qid)))
           (if qid
             qid
             (jsexpr-object-ref-recursive data '(meta qid)))))
        (else #f)))

(define document-root (config-document-root SERVER-CONFIG))

(define (response/jsexpr code message jse)
  (cons
    (response
      code
      message
      (current-seconds)
      mime:json
      '()
      (lambda (op)
        (write-bytes (jsexpr->bytes jse) op)))
    (get-resp-qid jse)))

(define (response/OK resp mime-type)
  (cons
    (response
      200 #"OK"
      (current-seconds)
      mime-type
      '()
      (lambda (op)
        (write-bytes resp op)))
    #f))

(define (response/OK/jsexpr jse)
  (response/jsexpr 200 #"OK" jse))

(define (response/bad-request jse)
  (response/jsexpr 400 #"Bad request" jse))

(define (response/bad-request/invalid-json)
  (response/bad-request (failure-response "Invalid JSON")))

(define (response/bad-request/invalid-post)
  (response/bad-request (failure-response "Invalid POST data")))

(define (response/not-found)
  (cons
    (response/xexpr
      #:code    404
      #:message #"Not found"
      `(html
         (body
           (p "Not found"))))
    #f))

(define (response/internal-error jse)
  (response/jsexpr 500 #"Internal server error" jse))

(define (response/internal-error/generic)
  (response/internal-error (failure-response "Internal server error")))

(define (response/internal-error/ars)
  (response/internal-error (failure-response "ARS could not process the request")))

(define (log-request req)
  (parameterize ((log:current-log-port (config-log-port SERVER-CONFIG)))
    (log:pretty-log (request->log req (config-log-level SERVER-CONFIG)))
    (flush-output (log:current-log-port))))

(define (request->log req log-level)
  (define (uri->log uri)
    `((scheme . ,(url-scheme uri))
      (host   . ,(url-host   uri))
      (port   . ,(url-port   uri))
      (path   . ,(path/param-path (car (url-path uri))))))

  (define (header->log header)
    `((field . ,(header-field header))
      (value . ,(header-value header))))

  (let ((base-log `(request ,log-level
                            (client-ip . ,(request-client-ip req))
                            (method    . ,(request-method req))
                            (uri       . ,(uri->log (request-uri req))))))
    (if (eq? log-level 'info)
      (append base-log
              `((headers   . ,(map header->log (request-headers/raw req)))
                (data      . ,(request-post-data/raw req))))
      base-log)))

(define (response/log resp-proc)
  (define-values (response resp-log)
    (response->response/log resp-proc (config-log-level SERVER-CONFIG)))
  (parameterize ((log:current-log-port (config-log-port SERVER-CONFIG)))
    (log:pretty-log resp-log)
    (flush-output (log:current-log-port)))
  response)

(define (response->response/log resp-proc log-level)
  (define is-info (eq? log-level 'info))
  (let*-values (((resp/qid time.cpu time.real time.gc)
                 (if is-info
                   (time-apply resp-proc '())
                   (values `(,(resp-proc)) #f #f #f)))
                ((resp qid)
                 (values (caar resp/qid) (cdar resp/qid))))
    (let ((base-log `(response ,log-level
                               (code . ,(response-code resp))
                               (uuid . ,qid))))
      (values
        resp
        (if is-info
          (append base-log
                  `((bytes-transferred. ,(bytes-length (call-with-output-bytes (response-output resp))))
                    (time-to-serve (cpu  . ,time.cpu)
                                   (real . ,time.real)
                                   (gc   . ,time.gc))))
          base-log)))))

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
  (log-request req)
  (define uri (request-uri req))
  (define resource (map path/param-path (url-path uri)))
  (define f (string-append document-root "build/" (string-join resource "/")))
  (file->response f))

(define (/index (req #f))
  (when req (log-request req))
  (define index.html (string-append document-root "build/index.html"))
  (file->response index.html))

;TODO: ARS is down (no response from ARS)
(define (make-query-endpoint qgraph->trapi-query)
  (lambda (req)
    (log-request req)
    (response/log
      (lambda ()
        (with-handlers ((exn:fail:read?
                          (lambda (e) (response/bad-request/invalid-json)))
                        (exn:fail?
                          (lambda (e) (response/internal-error/generic))))
          (let ((post-data (request-post-data/raw req)))
            (define trapi-query (and post-data (qgraph->trapi-query (bytes->jsexpr post-data))))
            (cond (trapi-query
                    (define post-resp (post-query trapi-query))
                    (match (car post-resp)
                      ('error (response/internal-error/ars))
                      (_  (response/OK/jsexpr (make-response "success" (cdr post-resp))))))
                  ((not post-data) (response/bad-request/invalid-post))
                  (else
                    (response/bad-request (failure-response "Query could not be converted to TRAPI"))))))))))

(define /query (make-query-endpoint qgraph->trapi-query))
(define /creative-query (make-query-endpoint disease->creative-query))

(define (make-result-endpoint pull-proc process-query-data)
  (lambda (req)
    (log-request req)
    (response/log
      (lambda ()
        (with-handlers ((exn:fail:read?
                          (lambda (e) (response/bad-request/invalid-json)))
                        (exn:fail?
                          (lambda (e) (response/internal-error/generic))))
          (let* ((post-data (request-post-data/raw req))
                 (qid (and post-data (get-req-qid (bytes->jsexpr post-data)))))
            (cond (qid (let ((query-state (pull-proc qid)))
                         (if query-state
                           (response/OK/jsexpr (make-response (query-state-status query-state)
                                                              (process-query-data qid (query-state-data query-state))))
                           (response/internal-error/generic))))
                  ((not post-data) (response/bad-request/invalid-post))
                  (else (response/internal-error/ars)))))))))

(define /result
  (make-result-endpoint
    pull-query-answers
    (lambda (qid answers) (trapi:answers->summary answers evidence-expanders))))

(define /creative-status
  (make-result-endpoint
    pull-query-status
    (lambda (qid answers) answers)))

(define /creative-result
  (make-result-endpoint
    pull-query-answers
    (lambda (qid answers)
      (trapi:creative-answers->summary qid answers))))

(define-values (dispatcher _)
    (dispatch-rules
      (("")                #:method "get"  /index)
      (("query")           #:method "post" /query)
      (("result")          #:method "post" /result)
      (("creative_query")  #:method "post" /creative-query)
      (("creative_status") #:method "post" /creative-status)
      (("creative_result") #:method "post" /creative-result)
      (else                handle-static-file-request)))

(serve/servlet dispatcher
    #:servlet-path ""
    #:servlet-regexp #rx""
    #:launch-browser? #f
    #:listen-ip #f
    #:port (config-port SERVER-CONFIG)
    #:safety-limits (make-safety-limits
                      #:response-timeout (config-response-timeout SERVER-CONFIG)))
