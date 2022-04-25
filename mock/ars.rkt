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
  (hash "3" '("test/ars/3-1.json" "test/ars/3-2.json" "test/ars/3-3.json")
  ))

(define (poster query)
  (cons 'done query))

(define (status-puller qid)
  (if (hash-has-key? mock-queries qid)
      'done
      'error))
(define (result-puller qid)
  (define result (hash-ref mock-queries qid))
  (map (lambda (path)
         (call-with-input-file (string-add-prefix (config-document-root server-config) path)
           (lambda (input-port)
              (jsexpr-object-ref (read-json input-port) 'message))))
       result))