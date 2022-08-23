#lang racket/base

(require
  json
  "../common.rkt"
  "../config.rkt")

(provide
  post-query
  pull-query-status
  pull-query-answers)

; Format:
;   qid '(<path to answer> ...)
(define mock-queries
  (hash
    ; Creative mode
    "MONDO:0005148" '("test/ars/res3-mondo0005148.json"
                      "test/ars/res6-mondo0005148.json"
                      "test/ars/res11-mondo0005148.json"
                      "test/ars/res12-mondo0005148.json"
                      "test/ars/res14-mondo0005148.json")
))

(define (post-query query)
  (cons 'done query))
(define (pull-query-status qid)
  (if (hash-has-key? mock-queries qid)
    (make-query-state "success"
                      (make-jsexpr-object `((qid . ,qid)
                                            (aras . ("mock")))))
    (make-query-state "error" (format "No key ~a" qid))))
(define (pull-query-answers qid)
  (define answers (hash-ref mock-queries qid))
  (make-query-state "success"
    (map (lambda (path)
          (call-with-input-file (string-add-prefix (config-document-root SERVER-CONFIG) path)
            (lambda (input-port)
                (make-answer (jsexpr-object-ref-recursive
                               (read-json input-port)
                               '(fields data message))
                             "mock"))))
        answers)))
