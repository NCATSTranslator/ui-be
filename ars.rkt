#lang racket/base

(require racket/match
         racket/pretty
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
(define (get-health resp) (car resp))

(define (parse-query-status resp)
  (let* ((fields (jsexpr-object-ref resp   'fields))
         (code   (jsexpr-object-ref fields 'code))
         (status (jsexpr-object-ref fields 'status)))
    (cons code status)))

(define (parse-submit-query-resp resp)
  (let* ((health (parse-query-status resp))
         (qid    (jsexpr-object-ref resp 'pk)))
    (cons health qid)))

(define (parse-query-result resp)
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

;(pretty-print (post-query (read-json (open-input-file "test/workflowA/A.0_RHOBTB2_direct.json"))))
;(pretty-print (pull-query-status "490236a7-ec86-4776-9a4d-d8d117b6df9f"))
(pretty-print (pull-query-result "490236a7-ec86-4776-9a4d-d8d117b6df9f"))