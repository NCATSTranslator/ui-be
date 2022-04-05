;TODO
; * Name resolution search should be injectable
;   - Change unit tests to use a mocked name resolution search
; * qnode->trapi-qnode: make more robust to missing ids/categories
#lang racket/base

(require 
  racket/string
  racket/match
  json
  "common.rkt"
  "curie-search.rkt")

(provide qgraph->trapi-query)

(define (index->node-id i) (string-add-prefix "n" (number->string i)))
(define (index->edge-id i) (string-add-prefix "e" (number->string i)))
(define (biolink-tag str) (string-add-prefix "biolink:" str))

(define (qnode->trapi-qnode qnode curie-searcher)
  (let loop ((trapi-qnode-alist '())
             (qnode-alist (jsexpr-object->alist qnode)))
    (if (null? qnode-alist)
        (make-immutable-hasheq trapi-qnode-alist)
        (let ((trapi-entry (match (car qnode-alist)
                            (`(type . ,type) `(categories . (,(biolink-tag type))))
                            (`(name . ,name) (and (not (empty-string? name))
                                                   `(ids . ,(curie-searcher name))))
                            (_ #f))))
          (loop (if trapi-entry
                    (cons trapi-entry trapi-qnode-alist)
                    trapi-qnode-alist)
                (cdr qnode-alist))))))

(define (qedge->trapi-qedge qedge)
  (hasheq 'subject    (index->node-id (jsexpr-object-ref qedge 'source))
          'object     (index->node-id (jsexpr-object-ref qedge 'target))
          'predicates `(,(biolink-tag (jsexpr-object-ref qedge 'type)))))
  
(define (qgraph->trapi-qgraph qgraph curie-searcher)
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
    (hasheq 'nodes (objs->trapi-objs 'nodes index->node-id (lambda (qnode)
                                                            (qnode->trapi-qnode qnode curie-searcher))) 
            'edges (objs->trapi-objs 'edges index->edge-id qedge->trapi-qedge))))

(define (qgraph->trapi-query qgraph (curie-searcher
                                      (lambda (name)
                                        (curie-search nr-searcher name 0 10))))
  (define trapi-qgraph (qgraph->trapi-qgraph qgraph curie-searcher))
  (and trapi-qgraph
       (hasheq 'message (hasheq 'query_graph trapi-qgraph))))

(define (trapi-result->summarization trapi-result trapi-knowledge-graph)
  (define (get-obj-binding-ids bindings) ; compare with map + flatten
    (map (lambda (binding)
           (cons (car binding)
                 (map (lambda (obj) (string->symbol (jsexpr-object-ref obj 'id)))
                      (cdr binding))))
         (jsexpr-object->alist bindings)))

  (define (process-obj-attributes obj-summary obj-attributes attr-mapping)
    (let loop ((attrs obj-attributes)
                (summary obj-summary))
      (if (null? attrs)
          summary
          (let* ((current-attr (car attrs))
                  (found-attr (member (jsexpr-object-ref current-attr 'attribute_type_id)
                                      attr-mapping
                                      (lambda (a b) (equal? a (car b))))))
            (loop (cdr attrs)
                  (if found-attr
                      (hash-set summary
                                (cdar found-attr)
                                (jsexpr-object-ref current-attr 'value))
                      summary))))))
  
  (define (get-obj-summary type obj-bindings obj-prop-mapping trapi-knowledge-graph)
    (define objs (jsexpr-object-ref trapi-knowledge-graph type)) 
    (map (lambda (obj-binding)
           (cons (car obj-binding)
                 (map (lambda (obj-id)
                        (define obj-data (jsexpr-object-ref objs obj-id))
                        (let loop ((prop-mapping obj-prop-mapping)
                                   (obj-summary (hash)))
                          (if (null? prop-mapping)
                              obj-summary 
                              (loop (cdr prop-mapping)
                                    (if (jsexpr-object-has-key? obj-data (caar prop-mapping))
                                        ((cdar prop-mapping)
                                          obj-summary
                                          (jsexpr-object-ref obj-data (caar prop-mapping)))
                                        obj-summary)))))
                      (cdr obj-binding))))
         obj-bindings))
  
  (define (prop-key-change new-key) (lambda (ns v) (hash-set ns new-key v)))
  (define (attribute-handler attribute-mapping)
    (lambda (ns v) (process-obj-attributes ns v attribute-mapping)))

  (define node-binding-ids (get-obj-binding-ids (jsexpr-object-ref trapi-result
                                                                   'node_bindings)))
  (displayln node-binding-ids)
  (displayln (get-obj-summary
               'nodes
               node-binding-ids
               `((name       . ,(prop-key-change 'name))
                 (categories . ,(prop-key-change 'type))
                 (attributes . ,(attribute-handler 
                                  `((,(biolink-tag "description") . description)))))
               trapi-knowledge-graph))
  
  (define edge-binding-ids (get-obj-binding-ids (jsexpr-object-ref trapi-result 'edge_bindings)))
  (displayln edge-binding-ids)
  (displayln (get-obj-summary
               'edges
               edge-binding-ids
               `((object     . ,(prop-key-change 'object))
                 (subject    . ,(prop-key-change 'subject))
                 (predicate  . ,(prop-key-change 'predicate))
                 (attributes . ,(attribute-handler
                                  `((,(biolink-tag "original_knowledge_source") . "source")))))
               trapi-knowledge-graph)))

(module+ test
  (require rackunit)
  (define test-qgraph (read-json (open-input-file "test/trapi/qgraph.json")))
  (define test-query-expected (read-json (open-input-file "test/trapi/query-expected.json")))
  (define (mock-curie-searcher name) '("NCBIGene:23221" "MONDO:0033373" "UMLS:C4693903"))
  (check-equal? (qgraph->trapi-query test-qgraph mock-curie-searcher) test-query-expected)

  (define test-invalid-qgraph (read-json (open-input-file "test/trapi/invalid-qgraph.json")))
  (check-false (qgraph->trapi-query test-invalid-qgraph))

  (define test-result-summarization (jsexpr-object-ref (read-json (open-input-file "test/ars/2-1.json"))
                                                       'message))
  (trapi-result->summarization (car (jsexpr-object-ref test-result-summarization 'results))
                               (jsexpr-object-ref test-result-summarization 'knowledge_graph))
)