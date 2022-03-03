; TODO
; * parse results
; * unit tests for parsers
; * handle parsing errors
; * handle network errors

#lang racket/base

(require racket/match
         racket/pretty
         racket/file
         net/http-client
         json)

(define (jsexpr-object-ref je k) (hash-ref je k))

(define ars-host "ars.transltr.io")
(define post-query-uri "/ars/api/submit")
(define pull-query-uri "/ars/api/messages/~a~a")

(define (ars-sendrecv uri (method #"GET") (data #f))
  (match-define-values (_ _ resp-out)
    (http-sendrecv ars-host
                   uri
                   #:ssl? #t
                   #:method method
                   #:data data))
  (read-json resp-out))

(define (get-code health) (car health))
(define (get-status health) (cdr health))
(define (query-done? health)
  (and (equal? (get-code health)   200)
       (equal? (get-status health) "Done")))
(define (parse-health jse)
  (let ((code (jsexpr-object-ref jse 'code))
        (status (jsexpr-object-ref jse 'status)))
    (cons code status)))


(define (parse-query-status resp)
  (parse-health (jsexpr-object-ref resp 'fields)))

(define (parse-submit-query-resp resp)
  (let* ((health (parse-query-status resp))
         (qid    (jsexpr-object-ref resp 'pk)))
    (cons health qid)))

(define (parse-query-result resp)
  (let ((status (jsexpr-object-ref resp 'status)))
    (if (equal? status "Done")
        (for/fold ((actor-data '()))
                  ((actor (jsexpr-object-ref resp 'children)))
          (if (query-done? (parse-health actor))
              (cons (pull-query-actor-result (jsexpr-object-ref actor 'message)) actor-data)
              actor-data))
        #f))) ;TODO: exception?

(define (parse-query-actor-result resp)
  resp)

(define (post-query query)
  (parse-submit-query-resp
    (ars-sendrecv post-query-uri
                  #"POST"
                  (jsexpr->bytes query))))

(define (pull-query qid (trace? #f))
  (define uri (format pull-query-uri qid (if trace? "?trace=y" "")))
  (ars-sendrecv uri))

(define (pull-query-status qid)
  (parse-query-status (pull-query qid)))

(define (pull-query-result qid)
  (parse-query-result (pull-query qid #t)))

(define (pull-query-actor-result qid)
  (parse-query-actor-result (pull-query qid)))

;(pretty-print (post-query (read-json (open-input-file "test/workflowA/A.0_RHOBTB2_direct.json"))))
;(pretty-print (pull-query-status "490236a7-ec86-4776-9a4d-d8d117b6df9f"))
(write-json
  (pull-query-result "490236a7-ec86-4776-9a4d-d8d117b6df9f")
  (open-output-file "./test/workflowA/resp/A.0_RHOBTB2_direct_children.json" #:exists 'replace))
#;(write-json
  (pull-query-actor-result "ada9d2b3-2a75-47f9-86e3-dbb295c1a0bb")
  (open-output-file "./A.0_RHOBTB2_direct_aragorn.json" #:exists 'replace))