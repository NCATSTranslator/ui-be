; TODO
; * unit tests for parsers
; * handle parsing errors
; * handle network errors

#lang racket/base

(require
  racket/match
  racket/pretty
  racket/file
  net/http-client
  json
  "common.rkt")

(provide 
  post-query
  pull-query-status
  pull-query-result)

(define ars-host "ars.transltr.io")
(define post-query-uri "/ars/api/submit")
(define (pull-query-uri qid trace?)
  (format "/ars/api/messages/~a~a" qid (if trace? "?trace=y" "")))

(define (ars-sendrecv uri (method #"GET") (data #f))
  (match-define-values (_ _ resp-out)
    (http-sendrecv ars-host
                   uri
                   #:ssl? #t
                   #:method method
                   #:data data))
  (read-json resp-out))

(define (get-results  jse) (jsexpr-object-ref jse 'results))
(define (get-message  jse) (jsexpr-object-ref jse 'message))
(define (get-fields   jse) (jsexpr-object-ref jse 'fields))
(define (get-status   jse) (jsexpr-object-ref jse 'status))
(define (get-code     jse) (jsexpr-object-ref jse 'code))
(define (get-data     jse) (jsexpr-object-ref jse 'data))
(define (get-children jse) (jsexpr-object-ref jse 'children))
(define (get-qid      jse) (jsexpr-object-ref jse 'pk))

(define (query-done? qstatus)
  (equal? qstatus 'done))



(define (parse-query-status resp)
  (match (cons (get-code resp) (get-status resp))
    ('(200 . "Done")    'done)
    ('(200 . "Running") 'running)
    (_                'error)))

(define (parse-submit-query-resp resp)
  (let* ((fields  (get-fields resp))
         (qstatus (parse-query-status fields))
         (qid     (get-qid resp)))
    (cons qstatus qid)))

(define (parse-query-result resp)
  (let ((status (get-status resp)))
    (if (equal? status "Done")
        (for/fold ((actors-data '()))
                  ((actor (get-children resp)))
          (define actor-data (and (query-done? (parse-query-status actor))
                                  (pull-query-actor-result (get-message actor))))
          (if actor-data
              (cons actor-data actors-data)
              actors-data))
        #f)))

(define (parse-query-actor-result resp)
  (let* ((fields (get-fields resp))
         (qstatus (parse-query-status fields)))
    (if (query-done? qstatus)
      (let* ((message (get-message (get-data fields)))
             (results (get-results message)))
        (and (not (null? results))
             message))
      qstatus)))
    

(define (post-query query)
  (parse-submit-query-resp
    (ars-sendrecv post-query-uri
                  #"POST"
                  (jsexpr->bytes query))))

(define (pull-query qid (trace? #f))
  (ars-sendrecv (pull-query-uri qid trace?)))

(define (pull-query-status qid)
  (parse-query-status (get-fields (pull-query qid))))

(define (pull-query-actor-result qid)
  (parse-query-actor-result (pull-query qid)))

(define (pull-query-result qid)
  (parse-query-result (pull-query qid #t)))