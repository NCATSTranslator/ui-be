;TODO
; See !!1
#lang racket/base

(require 
  racket/bool
  racket/string
  racket/match
  json
  "common.rkt"
  "curie-search.rkt")

(provide
  qgraph->trapi-query
  add-summary)

(define (index->node-id i) (string-add-prefix "n" (number->string i)))
(define (index->edge-id i) (string-add-prefix "e" (number->string i)))
(define (biolink-tag str) (string-add-prefix "biolink:" str))

; Extracting TRAPI properties and attributes
(define (make-mapping key transformer updater default)
  (lambda (obj)
    (define v (jsexpr-object-ref obj key #f))
    (lambda (acc) (updater (if v (transformer v) default) acc))))

(define (rename-and-change-property src-key tgt-key transformer)
  (make-mapping
    src-key 
    transformer
    (lambda (v obj) (jsexpr-object-set obj tgt-key v))
    'null))
(define (change-property key transformer)
  (rename-and-change-property key key transformer))
(define (rename-property src-key tgt-key)
  (rename-and-change-property src-key tgt-key (lambda (x) x)))
(define (get-property key)
  (rename-property key key))

(define (rename-attribute attribute-id tgt-key)
  (make-mapping
    'attributes
    (lambda (attributes)
      (let loop ((as attributes)
                 (result #f))
        (cond (result result)
              ((null? as) 'null)
              (else 
                (define a (car as))
                (loop (cdr as)
                      (and (equal? attribute-id (jsexpr-object-ref a 'attribute_type_id))
                           (jsexpr-object-ref a 'value)))))))
    (lambda (v obj) (jsexpr-object-set obj tgt-key v))
    'null))
(define (aggregate-attributes attribute-ids tgt-key)
  (make-mapping
    'attributes
    (lambda (attributes)
      (let loop ((as attributes)
                 (result '()))
        (cond ((null? as) result)
              (else
                (define a (car as))
                (define aid (member (jsexpr-object-ref a 'attribute_type_id) attribute-ids))
                (define v (if aid (jsexpr-object-ref a 'value) '()))
                (loop (cdr as)
                      (append result (if (list? v) v (list v))))))))
    (lambda (v obj) (jsexpr-object-set obj
                                       tgt-key
                                       (append (jsexpr-object-ref obj tgt-key '()) v)))
    '()))

(define (trapi-answer-query mappings)
  (lambda (obj)
    (map (lambda (mapping) (mapping obj))
         mappings)))

(define (id->link id)
  (match id
    ((pregexp "^PMC[0-9]+$") (pmc-id->pubmed-article-link id))
    ((pregexp "^PMID:")      (pm-id->pubmed-link (strip-id-tag id)))
    ((pregexp "^DOI:")       (doi-id->doi-link (strip-id-tag id)))
    (_ "Unknown supporting document type")))
(define (strip-id-tag id)
  (cadr (string-split id ":")))
(define (pmc-id->pubmed-article-link id) 
  (string-append "https://www.ncbi.nlm.nih.gov/pmc/articles/" id))
(define (pm-id->pubmed-link id)
  (string-append "https://pubmed.ncbi.nlm.nih.gov/" id))
(define (doi-id->doi-link id)
  (string-append "https://www.doi.org/" id))

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

(define (trapi-binding->kobj knowledge-graph binding type)
  (jsexpr-object-ref (jsexpr-object-ref knowledge-graph type) binding))
(define (trapi-edge-binding->trapi-kedge knowledge-graph edge-binding)
  (trapi-binding->kobj knowledge-graph edge-binding 'edges))
(define (trapi-node-binding->trapi-knode knowledge-graph node-binding)
  (trapi-binding->kobj knowledge-graph node-binding 'nodes))

(define (trapi-answers->summary trapi-answers primary-predicates node-query edge-query)
  (define (update-result-summary summary edge-summary)
    (define result-id (car edge-summary))
    (list (if result-id result-id (car summary))
          (append (cadr summary) (cadr edge-summary))
          (append (caddr summary) (caddr edge-summary)))) 
    
  (define (update-summary summary result-summary var-node)
    (define result-id (car result-summary)) ; Must have a result ID at this point to be valid
    (if result-id
      (let ((rs (if (jsexpr-object-has-key? summary result-id)
                    (jsexpr-object-ref summary result-id)
                    (hash var-node (hash)
                          'edge    (hash)))))
        (let loop ((node-updaters (cadr result-summary))
                   (edge-updaters (caddr result-summary))
                   (ns (jsexpr-object-ref rs var-node))
                   (es (jsexpr-object-ref rs 'edge)))
          (cond ((and (null? node-updaters) (null? edge-updaters))
                 (let ((result (hash-set (hash-set rs var-node ns) 'edge es)))
                   (hash-set summary result-id result)))
                ((null? node-updaters)
                 (loop '() (cdr edge-updaters) ns ((car edge-updaters) es)))
                ((null? edge-updaters)
                 (loop (cdr node-updaters) '() ((car node-updaters) ns) es))
                (else (loop (cdr node-updaters) (cdr edge-updaters)
                            ((car node-updaters) ns) ((car edge-updaters) es))))))
      summary))

  (define (summarize-edge edge-binding var-node knowledge-graph)
    (define kedge (trapi-edge-binding->trapi-kedge knowledge-graph edge-binding))
    (define node-binding (string->symbol (jsexpr-object-ref kedge var-node)))
    (define primary-predicate (member (jsexpr-object-ref kedge 'predicate) primary-predicates))
    (displayln primary-predicate)
    (list (and primary-predicate (cons (car primary-predicate) node-binding))
          (node-query (trapi-node-binding->trapi-knode knowledge-graph node-binding))
          (edge-query kedge)))

  (define (summarize-result result var-node knowledge-graph)
    (let loop ((edge-bindings (map (lambda (b)
                                     (string->symbol (jsexpr-object-ref (car b) 'id)))
                                     (jsexpr-object-values (jsexpr-object-ref result 'edge_bindings))))
               (summary (list #f '() '())))
      (if (null? edge-bindings)
          summary
          (loop (cdr edge-bindings)
                (update-result-summary
                  summary
                  (summarize-edge (car edge-bindings) var-node knowledge-graph))))))
  
  (define (summarize-answer answer summary)
    (define qgraph (jsexpr-object-ref answer 'query_graph))
    (define kgraph (jsexpr-object-ref answer 'knowledge_graph))
    (define qnodes (jsexpr-object-ref qgraph 'nodes))
    (define qedges (jsexpr-object-ref qgraph 'edges))
    (define qedge (jsexpr-object-ref qedges 'e01))
    (define var-node (if (jsexpr-object-has-key?
                          (jsexpr-object-ref qnodes 
                            (string->symbol (jsexpr-object-ref qedge 'object)))
                          'ids)
                        'subject
                        'object))
    (let loop ((results (jsexpr-object-ref answer 'results))
              (summary summary))
      (if (null? results)
          summary
          (loop (cdr results)
                (update-summary summary (summarize-result (car results) var-node kgraph) var-node)))))
  
  (let loop ((answers trapi-answers)
             (summary (jsexpr-object)))
    (if (null? answers)
        (jsexpr-object-values summary)
        (loop (cdr answers)
              (summarize-answer (car answers) summary)))))

; TODO !!1
; * post processing
;   - all evidence needs to be distinct (some evidence needs to be split)
;   - all evidence needs to be converted to URLs
;   - add hard coded elements for toxicity and tags
; * fda 
;   - make utility updater to add by a path of keys to support internal fda structure
;   - figure out mapping (fda_status -> <integer>)
; * secondary predicates
;   - drop?
;   - figure out why it is including the secondary predicate in one of the summarized results
(define (add-summary result)
  result)


(module+ test
  (require rackunit)
  (define test-qgraph (read-json (open-input-file "test/trapi/qgraph.json")))
  (define test-query-expected (read-json (open-input-file "test/trapi/query-expected.json")))
  (define (mock-curie-searcher name) '("NCBIGene:23221" "MONDO:0033373" "UMLS:C4693903"))
  (check-equal? (qgraph->trapi-query test-qgraph mock-curie-searcher) test-query-expected)

  (define test-invalid-qgraph (read-json (open-input-file "test/trapi/invalid-qgraph.json")))
  (check-false (qgraph->trapi-query test-invalid-qgraph))

  (define test-result-summarization (read-json (open-input-file "test-local/workflowA/resp/A.0_RHOBTB2_direct_result.json")))
  (define summary (trapi-answers->summary test-result-summarization
                                          `(,(biolink-tag "entity_negatively_regulates_entity")
                                            ,(biolink-tag "increases_expression_of")
                                           )
                                          (trapi-answer-query
                                            `(,(get-property 'name)
                                              ,(rename-property 'categories 'types)
                                              ,(rename-attribute
                                                (biolink-tag "highest_FDA_approval_status")
                                                'highest_fda_approval_status))
                                          )
                                          (trapi-answer-query
                                            `(,(get-property 'predicate)
                                              ,(aggregate-attributes
                                                `(,(biolink-tag "supporting_document")
                                                  ,(biolink-tag "Publication")
                                                  ,(biolink-tag "publications")
                                                 )
                                                'evidence)
                                          ))))
  (write-json summary (open-output-file "test-local/example-summary.json" #:exists 'replace))
)