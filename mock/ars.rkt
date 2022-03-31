#lang racket/base

(require
  json
  "../common.rkt"
  "../config.rkt")

(provide
  poster
  status-puller
  result-puller)

; Format:
;   qid '(<path to result> ...)
(define mock-queries
  (hash "1" '("test/ars/1-1.json" "test/ars/1-2.json")
        "2" '("test/ars/2-1.json")
  ))

(define (poster query) "mock query posted")
(define (status-puller qid)
  (if (hash-has-key? mock-queries qid)
      'done
      'error))
(define (result-puller qid)
  (define result (hash-ref mock-queries qid))
  (map (lambda (path)
         (jsexpr-object-ref
           (read-json
             (open-input-file (string-add-prefix (config-document-root server-config) path)))
           'message))
       result))