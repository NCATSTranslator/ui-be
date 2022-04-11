;TODO
; * Name resolution search should be injectable
;   - Change unit tests to use a mocked name resolution search
; * qnode->trapi-qnode: make more robust to missing ids/categories
#lang racket/base

(require 
  racket/bool
  racket/string
  racket/match
  json
  "common.rkt"
  "curie-search.rkt")

(provide qgraph->trapi-query)

(define (index->node-id i) (string-add-prefix "n" (number->string i)))
(define (index->edge-id i) (string-add-prefix "e" (number->string i)))
(define (biolink-tag str) (string-add-prefix "biolink:" str))

; Extracting TRAPI properties and attributes
(define (trapi-obj-property-transformer old-key new-key f (default 'null))
  (lambda (obj)
    (define v (jsexpr-object-ref obj old-key #f))
    (cons new-key (if v (f v) default))))
(define (get-prop k)
  (trapi-obj-property-transformer k k (lambda (x) x)))
(define (rename-prop ok nk)
  (trapi-obj-property-transformer ok nk (lambda (x) x)))

(define (attribute-transformer aid f (default 'null))
  (lambda (obj)
    (define v (and (equal? (jsexpr-object-ref obj 'attribute_type_id) aid)
                           (jsexpr-object-ref obj 'value)))
    (if v (f v) default)))
(define (identity-attribute-transformer aid)
  (attribute-transformer aid (lambda (x) x)))
(define (attribute-renamer new-aid attrs t)
  (cons new-aid
        (if attrs
            (let loop ((as attrs)
                      (res 'null))
              (if (or (null? as) (not (jsexpr-null? res)))
                  res
                  (loop (cdr as)
                        (t (car as)))))
            'null)))
(define (attribute-accumulator new-aid attrs transformers)
  (cons new-aid
        (foldl (lambda (attr acc)
                 (let loop ((ts transformers)
                            (result acc))
                   (if (null? ts)
                       result 
                       (loop (cdr ts)
                             (let ((v ((car ts) attr)))
                               (cond ((or (jsexpr-null? v) (false? v)) result)
                                     ((list? v) (append result v))
                                     (else (cons v result))))))))
               '()
               (if attrs attrs '()))))

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

(define (make-mapping key transformer updater default)
  (lambda (obj)
    (define v (jsexpr-object-ref obj key #f))
    (lambda (acc) (updater (if v (transformer v) default) acc))))

(define (make-processor mappings)
  (lambda (obj)
    (map (lambda (mapping) (mapping obj))
         mappings)))

(define (trapi-answers->summary trapi-answers primary-predicates node-processor edge-processor)
  (define (update-result-summary summary edge-summary)
    (define result-id (car edge-summary))
    (list (if result-id result-id (car summary))
          (append (cadr summary) (cadr edge-summary))
          (append (caddr summary) (caddr edge-summary)))) 
    
  (define (update-summary summary result-summary var-node)
    (displayln result-summary)
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
    (list (and primary-predicate (cons (car primary-predicate) node-binding))
          (node-processor (trapi-node-binding->trapi-knode knowledge-graph node-binding))
          (edge-processor kedge)))

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
                                          `(,(biolink-tag "entity_negatively_regulates_entity"))
                                          (make-processor
                                            `(,(make-mapping 'name
                                                             identity
                                                             (lambda (v jse) (jsexpr-object-set jse 'name v))
                                                             'null)))
                                          (make-processor
                                            `(,(make-mapping 'predicate
                                                             identity
                                                             (lambda (v jse) (jsexpr-object-set jse 'predicate v))
                                                             'null)))))
  (write-json summary (open-output-file "test-local/example-summary.json" #:exists 'replace))
)