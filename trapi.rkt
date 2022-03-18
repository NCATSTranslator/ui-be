;TODO
; * Name resolution search should be injectable
;   - Change unit tests to use a mocked name resolution search
; * qnode->trapi-qnode: make more robust to missing ids/categories
#lang racket/base

(require 
  racket/string
  json
  "common.rkt"
  "curie-search.rkt")

(provide qgraph->trapi-qgraph)

(define (index->node-id i) (string-add-prefix "n" (number->string i)))
(define (index->edge-id i) (string-add-prefix "e" (number->string i)))
(define (biolink-tag str) (string-add-prefix "biolink:" str))

(define (qnode->trapi-qnode qnode)
  (if (empty-string? (jsexpr-object-ref qnode 'name))
      (hasheq 'categories `(,(biolink-tag (jsexpr-object-ref qnode 'type))))
      (hasheq 'ids (curie-search nr-searcher (jsexpr-object-ref qnode 'name) 0 10)
              'categories `(,(biolink-tag (jsexpr-object-ref qnode 'type))))))

(define (qedge->trapi-qedge qedge)
  (hasheq 'subject (index->node-id (jsexpr-object-ref qedge 'source))
          'object  (index->node-id (jsexpr-object-ref qedge 'target))
          'predicates `(,(biolink-tag (jsexpr-object-ref qedge 'type)))))
  
(define (qgraph->trapi-qgraph qgraph)
  (with-handlers ((exn:fail:contract? (lambda (e) #f)))
    (define (objs->trapi-objs key id-generator obj-converter)
      (let loop ((os (jsexpr-object-ref qgraph key))
                  (i 0)
                  (trapi-os '()))
        (if (null? os)
            (make-immutable-hasheq trapi-os)
            (loop (cdr os)
                  (+ i 1)
                  (cons `(,(string->symbol (id-generator i)) . ,(obj-converter (car os)))
                        trapi-os)))))
    (hasheq 'nodes (objs->trapi-objs 'nodes index->node-id qnode->trapi-qnode) 
            'edges (objs->trapi-objs 'edges index->edge-id qedge->trapi-qedge))))
    

(module+ test
  (require rackunit)
  (define test-qgraph (read-json (open-input-file "test/trapi/qgraph.json")))
  (define test-qgraph-expected (read-json (open-input-file "test/trapi/qgraph-expected.json")))
  (check-equal? (qgraph->trapi-qgraph test-qgraph) test-qgraph-expected)

  (define test-invalid-qgraph (read-json (open-input-file "test/trapi/invalid-qgraph.json")))
  (check-false (qgraph->trapi-qgraph test-invalid-qgraph))
)