; TODO
; * unit tests for parsers
; * handle parsing errors
; * handle network errors

#lang racket/base

(provide
  post-query
  pull-query-status
  pull-query-result)

(require
  racket/string
  racket/match
  racket/pretty
  racket/file
  net/http-client
  json
  "common.rkt"
  "config.rkt"
  (prefix-in trapi: "trapi.rkt"))

(define ars-host (ars-config 'host))
(define ars-post-uri (ars-config 'post-uri))
(define (ars-pull-uri qid trace?)
  (format "~a/~a~a" (ars-config 'pull-uri)
                    qid
                    (if trace?
                        (make-url-params '(("trace" . "y")))
                        "")))

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
(define (get-agent    jse) (jsexpr-object-ref-recursive jse '(actor agent)))

(define (query-done? qstatus)
  (equal? qstatus 'done))

(define (resp-okay? resp)
  (let ((status (get-status resp)))
    (or (equal? status "Done") (equal? status "Running"))))

(define (qstatus->status qstatus)
  (case qstatus
    (("Done") "success")
    (("Running") "running")
    (else (raise exn:fail))))

(define (post-query query)
  (parse-submit-query-resp
    (ars-sendrecv ars-post-uri #"POST" (jsexpr->bytes query))))

(define (pull-query-status qid)
  (parse-query-metadata
    (ars-sendrecv (ars-pull-uri qid #t))))

(define (pull-query-result qid)
  (parse-query-result
    (ars-sendrecv (ars-pull-uri qid #t))))

(define (parse-query-status resp)
  (match (cons (get-code resp) (get-status resp))
    ('(200 . "Done")    'done)
    ('(202 . "Running") 'running)
    (_                  'error)))

(define (parse-query-metadata resp)
  (and (resp-okay? resp)
       (make-query-state (qstatus->status (get-status resp))
                         (trapi:metadata-object
                           (get-message resp)
                           (foldl (lambda (actor agents)
                                   (if (query-done? (parse-query-status actor))
                                     (cons (get-agent actor) agents)
                                     agents))
                                 '()
                                 (get-children resp))))))

(define (parse-submit-query-resp resp)
  (let* ((fields  (get-fields resp))
         (qstatus (parse-query-status fields))
         (qid     (get-qid resp)))
    (cons qstatus qid)))

(define (parse-query-result resp)
  (define (pull-query-actor-result qid)
    (parse-query-actor-result
      (ars-sendrecv (ars-pull-uri qid #f))))

  (and (resp-okay? resp)
       (make-query-state (qstatus->status (get-status resp))
                         (foldl (lambda (actor answers)
                                   (let ((actor-data (and (query-done? (parse-query-status actor))
                                                         (pull-query-actor-result (get-message actor)))))
                                     (if actor-data
                                       (cons (make-answer actor-data (get-agent actor)) answers)
                                       answers)))
                                 '()
                                 (get-children resp)))))

(define (parse-query-actor-result resp)
  (with-handlers ((exn:fail? (lambda (e) #f))) ; Some ARAs report done when not done
    (let* ((fields (get-fields resp))
           (qstatus (parse-query-status fields)))
      (if (query-done? qstatus)
        (let* ((message (get-message (get-data fields)))
               (results (get-results message)))
          (and (not (null? results))
              message))
        qstatus))))
