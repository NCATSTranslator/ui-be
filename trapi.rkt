;TODO
; See !!1
#lang racket/base

(require 
  racket/bool
  racket/string
  racket/match
  racket/function
  racket/list
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

(define (rename-and-change-property src-key kpath transformer)
  (make-mapping
    src-key 
    transformer
    (lambda (v obj) (jsexpr-object-set-recursive obj kpath v))
    'null))
(define (change-property key transformer)
  (rename-and-change-property key (list key) transformer))
(define (rename-property key kpath)
  (rename-and-change-property key kpath identity))
(define (get-property-when key update?)
  (make-mapping
    key
    identity
    (lambda (v obj) (if (update? v)
                        (jsexpr-object-set obj key v)
                        obj))
    'null))
(define (get-property key)
  (rename-property key (list key)))

(define (rename-and-transform-attribute attribute-id kpath transform)
  (make-mapping
    'attributes
    (lambda (attributes)
      (let loop ((as attributes)
                 (result #f))
        (cond (result (transform result))
              ((null? as) 'null)
              (else 
                (define a (car as))
                (loop (cdr as)
                      (and (equal? attribute-id (jsexpr-object-ref a 'attribute_type_id))
                           (jsexpr-object-ref a 'value)))))))
    (lambda (v obj) (jsexpr-object-set-recursive obj kpath v))
    'null))
(define (rename-attribute attribute-id kpath)
  (rename-and-transform-attribute attribute-id kpath identity))
(define (aggregate-and-transform-attributes attribute-ids tgt-key transform)
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
                      (append result (transform v)))))))
    (lambda (v obj) (jsexpr-object-set obj
                                       tgt-key
                                       (append (jsexpr-object-ref obj tgt-key '()) v)))
    '()))

(define (aggregate-attributes attribute-ids tgt-key)
  (aggregate-and-transform-attributes
    attribute-ids
    tgt-key
    (lambda (v) (if (list? v) v (list v)))))

(define (make-summarize-rules rules)
  (lambda (obj)
    (map (lambda (rule) (rule obj))
         rules)))

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

(define (fda-description->fda-level description)
  1)

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

(define (trapi-answers->summary trapi-answers primary-predicates node-query edge-query post-processing)
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
    (define qedge  (cdar (jsexpr-object->alist qedges)))
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
  
  (map post-processing
       (let loop ((answers trapi-answers)
                 (summary (jsexpr-object)))
         (if (null? answers)
             (jsexpr-object-values summary)
             (loop (cdr answers)
                   (summarize-answer (car answers) summary))))))

(define (add-summary result)
  (define primary-predicates `(,(biolink-tag "treats")))
  (define (remove-duplicate-evidence answer)
    (jsexpr-object-set-recursive
      answer
      '(edge evidence)
      (remove-duplicates
        (jsexpr-object-ref-recursive
          answer
          '(edge evidence)))))
  (define fda-path '(fda_info highest_fda_approval_status))
  (define (add-toxicity-info answer)
    (jsexpr-object-set-recursive
      answer
      '(subject toxicity_info level)
      "Low"))
  (define (add-last-publication_date answer)
    (jsexpr-object-set-recursive answer '(edge last_publication_date) "1/1/2022"))
  (define summary
    (trapi-answers->summary
      (jsexpr-object-ref result 'data)
      primary-predicates
      (make-summarize-rules ; node rules
        `(,(get-property 'name)
          ,(rename-property 'categories '(types))
          ,(rename-and-transform-attribute
            (biolink-tag "highest_FDA_approval_status")
            '(fda_info)
            (lambda (fda-description)
              (hash 'highest_fda_approval_status fda-description
                    'max_level (fda-description->fda-level fda-description))))))
      (make-summarize-rules ; edge rules
        `(,(get-property-when
            'predicate
            (lambda (p) (member p primary-predicates)))
          ,(aggregate-and-transform-attributes
            `(,(biolink-tag "supporting_document")
              ,(biolink-tag "Publication")
              ,(biolink-tag "publications"))
            'evidence
            (lambda (evidence)
              (if (list? evidence)
                  (map id->link evidence)
                  (map id->link (string-split evidence "|")))))))
      (lambda (answer) ; answer post-processing 
        (add-last-publication_date
          (remove-duplicate-evidence
            (add-toxicity-info answer))))))
  (jsexpr-object-set result 'summary summary))



(module+ test
  (require rackunit)
  (define test-qgraph (read-json (open-input-file "test/trapi/qgraph.json")))
  (define test-query-expected (read-json (open-input-file "test/trapi/query-expected.json")))
  (define (mock-curie-searcher name) '("NCBIGene:23221" "MONDO:0033373" "UMLS:C4693903"))
  (check-equal? (qgraph->trapi-query test-qgraph mock-curie-searcher) test-query-expected)

  (define test-invalid-qgraph (read-json (open-input-file "test/trapi/invalid-qgraph.json")))
  (check-false (qgraph->trapi-query test-invalid-qgraph))
)