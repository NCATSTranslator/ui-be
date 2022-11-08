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
    "HP:0000822" '("test/ars/res1-hp0000822.json"
                   "test/ars/res6-hp0000822.json"
                   "test/ars/res8-hp0000822.json")
    "HP:0001993" '("test/ars/res14-hp0001993.json"
                   "test/ars/res7-hp0001993.json")
    "HP:0002014" '("test/ars/res9-hp0002014.json")
    "HP:0002315" '("test/ars/res12-hp0002315.json"
                   "test/ars/res7-hp0002315.json")
    "HP:0003003" '("test/ars/res10-hp0003003.json"
                   "test/ars/res12-hp0003003.json"
                   "test/ars/res14-hp0003003.json"
                   "test/ars/res5-hp0003003.json")
    "HP:0003124" '("test/ars/res0-hp0003124.json")
    "HP:0011015" '("test/ars/res2-hp0011015.json")
    "MONDO:0002251" '("test/ars/res11-mondo0002251.json"
                      "test/ars/res13-mondo0002251.json"
                      "test/ars/res9-mondo0002251.json")
    "MONDO:0004952" '("test/ars/res0-mondo0004952.json"
                      "test/ars/res13-mondo0004952.json"
                      "test/ars/res14-mondo0004952.json")
    "MONDO:0004975" '("test/ars/res11-mondo0004975.json"
                      "test/ars/res14-mondo0004975.json"
                      "test/ars/res2-mondo0004975.json"
                      "test/ars/res7-mondo0004975.json")
    "MONDO:0005002" '("test/ars/res1-mondo0005002.json"
                      "test/ars/res11-mondo0005002.json"
                      "test/ars/res7-mondo0005002.json"
                      "test/ars/res8-mondo0005002.json")
    "MONDO:0005147" '("test/ars/res1-mondo0005147.json"
                      "test/ars/res2-mondo0005147.json"
                      "test/ars/res3-mondo0005147.json"
                      "test/ars/res4-mondo0005147.json")
    "MONDO:0005148" '("test/ars/res3-mondo0005148.json"
                      "test/ars/res6-mondo0005148.json"
                      "test/ars/res11-mondo0005148.json"
                      "test/ars/res12-mondo0005148.json"
                      "test/ars/res14-mondo0005148.json")
    "MONDO:0005155" '("test/ars/res0-mondo0005155.json"
                      "test/ars/res12-mondo0005155.json"
                      "test/ars/res2-mondo0005155.json"
                      "test/ars/res7-mondo0005155.json")
    "MONDO:0005377" '("test/ars/res0-mondo0005377.json"
                      "test/ars/res1-mondo0005377.json"
                      "test/ars/res10-mondo0005377.json"
                      "test/ars/res5-mondo0005377.json"
                      "test/ars/res7-mondo0005377.json")
    "MONDO:0005812" '("test/ars/res0-mondo0005812.json"
                      "test/ars/res4-mondo0005812.json")
    "MONDO:0007947" '("test/ars/res10-mondo0007947.json"
                      "test/ars/res11-mondo0007947.json"
                      "test/ars/res6-mondo0007947.json")
    "MONDO:0008170" '("test/ars/res11-mondo0008170.json"
                      "test/ars/res3-mondo0008170.json")
    "MONDO:0010200" '("test/ars/res11-mondo0010200.json"
                      "test/ars/res4-mondo0010200.json")
    "empty" '()
    "null-attrs" '("test/ars/null-attrs.json")
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
