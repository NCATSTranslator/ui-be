;TODO
; Create FDA mapping
#lang racket/base

(provide
  qgraph->trapi-query
  disease->creative-query
  answers->summary
  creative-answers->summary
  metadata-object)

(require
  racket/bool
  racket/string
  racket/match
  racket/function
  racket/list
  racket/set
  json
  "common.rkt"
  "config.rkt"
  "curie-search.rkt"
  "evidence.rkt"

  racket/pretty)

(define (metadata-object qid agents)
  (make-jsexpr-object `((qid  . ,qid)
                        (aras . ,agents))))

(define (index->node-id i) (string-add-prefix "n" (number->string i)))
(define (index->edge-id i) (string-add-prefix "e" (number->string i)))

; Extracting TRAPI properties and attributes
(define (make-mapping key transformer updater default)
  (lambda (obj)
    (define v (jsexpr-object-ref obj key #f))
    (lambda (acc) (updater (if v (transformer v) default) acc))))

(define (rename-and-transform-property src-key kpath transformer)
  (make-mapping
    src-key
    transformer
    (lambda (v obj) (jsexpr-object-set-recursive obj kpath v))
    'null))
(define (transform-property key transformer)
  (rename-and-transform-property key (list key) transformer))
(define (rename-property key kpath)
  (rename-and-transform-property key kpath identity))
(define (get-property-when key update?)
  (make-mapping
    key
    identity
    (lambda (v obj)
      (define cv (jsexpr-object-ref obj key))
      (cond ((update? v) (jsexpr-object-set obj key v))
            (cv obj)
            (else (jsexpr-object-set obj key 'null))))
    'null))
(define (get-property key)
  (rename-property key (list key)))
(define (aggregate-property-when src-key kpath update?)
  (make-mapping
    src-key
    identity
    (lambda (v obj)
      (define cv (jsexpr-object-ref-recursive obj kpath))
      (cond ((update? v)
             (jsexpr-object-set-recursive
               obj
               kpath
               (append (if cv cv '()) (if (list? v) v (list v)))))
            (cv obj)
            (else (jsexpr-object-set-recursive obj kpath '()))))
    '()))
(define (aggregate-property src-key kpath)
  (aggregate-property-when src-key kpath (lambda (_) #t)))

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

(define (trapi-qgraph->trapi-message trapi-qgraph)
  (hasheq 'message (hasheq 'query_graph trapi-qgraph)))

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
  (and trapi-qgraph (trapi-qgraph->trapi-message trapi-qgraph)))

(define (disease->creative-query disease-obj)
  (define (disease->trapi-qgraph disease)
    (hasheq 'nodes
            (hasheq 'drug
                    (hasheq 'categories `(,(biolink-tag "ChemicalEntity")))
                    'disease
                    (hasheq 'ids        `(,disease)
                            'categories `(,(biolink-tag "Disease"))))
            'edges
            (hasheq 'treats
                    (hasheq 'subject        "drug"
                            'object         "disease"
                            'predicates     `(,(biolink-tag "treats"))
                            'knowledge_type "inferred"))))

  (hasheq 'message (hasheq 'query_graph (disease->trapi-qgraph (jsexpr-object-ref disease-obj 'disease)))))

(define (trapi-binding->kobj knowledge-graph binding type)
  (jsexpr-object-ref (jsexpr-object-ref knowledge-graph type) binding))
(define (trapi-edge-binding->trapi-kedge knowledge-graph edge-binding)
  (trapi-binding->kobj knowledge-graph edge-binding 'edges))
(define (trapi-node-binding->trapi-knode knowledge-graph node-binding)
  (trapi-binding->kobj knowledge-graph node-binding 'nodes))

(define (get-binding-id bindings key)
  (string->symbol (jsexpr-object-ref (car (jsexpr-object-ref bindings key)) 'id)))
(define (flatten-bindings bindings)
  (foldl (lambda (binding ids)
            (append ids (jsexpr-map (lambda (obj) (string->symbol (jsexpr-object-ref obj 'id)))
                                    binding)))
          '() (jsexpr-object-values bindings)))

; An rgraph is a list of nodes and a list of directed edges
(define (make-rgraph nodes edges) (cons nodes edges))
(define (rgraph-nodes rgraph) (car rgraph))
(define (rgraph-edges rgraph) (cdr rgraph))
(define (trapi-result->rgraph trapi-result)
  (make-rgraph (flatten-bindings (jsexpr-object-ref trapi-result 'node_bindings))
               (flatten-bindings (jsexpr-object-ref trapi-result 'edge_bindings))))
(define (rnode->key rnode kgraph) rnode)
(define (redge->key redge kgraph)
  (let ((kedge (trapi-edge-binding->trapi-kedge kgraph redge)))
    (path->key (list (jsexpr-object-ref kedge 'subject)
                     (jsexpr-object-ref kedge 'predicate)
                     (jsexpr-object-ref kedge 'object)))))

(define (make-redge->edge-id rgraph kgraph)
  (define redge->edge-id
    (make-immutable-hash
      (map (lambda (eid)
             (let ((kedge (trapi-edge-binding->trapi-kedge kgraph eid)))
               (cons eid (cons (string->symbol (jsexpr-object-ref kedge 'subject))
                               (string->symbol (jsexpr-object-ref kedge 'object))))))
           (rgraph-edges rgraph))))

  (lambda (redge)
    (hash-ref redge->edge-id redge)))

(define (make-rnode->out-edges rgraph kgraph)
  (define redge->edge-id (make-redge->edge-id rgraph kgraph))
  (define rnode->out-edges
    (let loop ((edges (rgraph-edges rgraph))
               (res (hash)))
      (if (null? edges)
        res
        (loop (cdr edges)
              (let* ((redge (car edges))
                     (edge-id (redge->edge-id redge))
                     (subject (car edge-id))
                     (object (cdr edge-id)))
                (hash-set res
                          subject
                          (cons (cons redge object) (hash-ref res subject '()))))))))
  (lambda (rnode)
    ; By defaulting to an empty list we are saying to ignore all paths that are not the
    ; terminal node and where the node does not appear in any edge where the node is the
    ; subject.
    (hash-ref rnode->out-edges rnode '())))

(define (rgraph-fold proc init acc)
  (let loop ((obj-left init)
             (res acc))
    (if (null? obj-left)
      res
      (let* ((expansion (proc (car obj-left)))
             (new-obj-left (car expansion))
             (new-res (cdr expansion)))
        (loop (append new-obj-left (cdr obj-left))
              (append new-res res))))))

(define (make-summary-fragment paths nodes edges)
  (list paths nodes edges))
(define (summary-fragment-paths summary-fragment)
  (car summary-fragment))
(define (summary-fragment-nodes summary-fragment)
  (cadr summary-fragment))
(define (summary-fragment-edges summary-fragment)
  (caddr summary-fragment))
(define empty-summary-fragment (make-summary-fragment '() '() '()))

(define (make-condensed-summary agent summary-fragment)
  (cons agent summary-fragment))
(define empty-condensed-summary '())
(define (condensed-summary-agent cs)
  (car cs))
(define (condensed-summary-fragment cs)
  (cdr cs))
(define (condensed-summary-paths cs)
  (summary-fragment-paths (condensed-summary-fragment cs)))
(define (condensed-summary-nodes cs)
  (summary-fragment-nodes (condensed-summary-fragment cs)))
(define (condensed-summary-edges cs)
  (summary-fragment-edges (condensed-summary-fragment cs)))
(define (path->key path)
  (string->symbol (number->string (equal-hash-code path))))

(define (merge-summary-attrs r s)
  (map (lambda (r-attr s-attr) (append r-attr s-attr)) r s))

(define (creative-answers->summary qid answers)
  (define node-rules
    (make-summarize-rules
      `(,(aggregate-property 'name '(names))
         ,(aggregate-property 'categories '(types))
         ,(aggregate-attributes
            `(,(biolink-tag "xref"))
            'curies)
         ,(rename-and-transform-attribute
            (biolink-tag "highest_FDA_approval_status")
            '(fda_info)
            (lambda (fda-description)
              (hash 'highest_fda_approval_status fda-description
                    'max_level (fda-description->fda-level fda-description))))
         ,(aggregate-attributes
            `(,(biolink-tag "description"))
            'description)
         ,(aggregate-attributes
            `(,(biolink-tag "synonym"))
            'synonym)
         ,(aggregate-attributes
            `(,(biolink-tag "same_as"))
            'same_as)
         ,(aggregate-attributes
            `(,(biolink-tag "IriType"))
            'iri_type))))

  (define edge-rules
    (make-summarize-rules
      `(,(aggregate-property 'predicate '(predicates))
         ,(get-property 'subject)
         ,(get-property 'object)
         ,(aggregate-attributes
            `(,(biolink-tag "IriType"))
            'iri_type)
         ,(aggregate-and-transform-attributes
            '("bts:sentence")
            'snippets
            (lambda (snippets)
              (if (list? snippets)
                snippets
                (jsexpr-object->alist snippets))))
         ,(aggregate-and-transform-attributes
            `(,(biolink-tag "supporting_document")
               ,(biolink-tag "Publication")
               ,(biolink-tag "publications"))
            'publications
            (lambda (evidence)
              (if (list? evidence)
                evidence
                (string-split evidence #rx",|\\|")))))))

  (condensed-summaries->summary
    qid
    (creative-answers->condensed-summaries
      answers
      node-rules
      edge-rules)))

(define (creative-answers->condensed-summaries answers node-rules edge-rules)
  (define (trapi-result->summary-fragment trapi-result kgraph)
    (define node-bindings (jsexpr-object-ref trapi-result 'node_bindings))
    (define rgraph (trapi-result->rgraph trapi-result))
    (define drug    (get-binding-id node-bindings 'drug))
    (define disease (get-binding-id node-bindings 'disease))
    (define rnode->out-edges (make-rnode->out-edges rgraph kgraph))
    (define rgraph-paths
      (rgraph-fold (lambda (path)
                     (let ((current-rnode (car path)))
                       (if (equal? current-rnode disease)
                         (cons '() `(,path))
                         (cons (filter (lambda (p) p)
                                       (map (lambda (e)
                                              (let ((next-edge (car e))
                                                    (next-node (cdr e)))
                                                (if (member next-node path)
                                                  #f
                                                  (cons next-node (cons next-edge path)))))
                                       (rnode->out-edges current-rnode)))
                               '()))))
                   `((,drug))
                   '()))

    ; Return a pair (key, update) for a node
    (define (summarize-rnode rnode kgraph)
      (cons (rnode->key rnode kgraph) (node-rules (trapi-node-binding->trapi-knode kgraph rnode))))

    ; Return a pair (key, update) for an edge
    (define (summarize-redge redge kgraph)
      (let ((kedge (trapi-edge-binding->trapi-kedge kgraph redge)))
        (cons (redge->key redge kgraph) (edge-rules kedge))))

    ; Convert paths structure to normalized node and edge IDs
    (define (normalize-paths rgraph-paths kgraph)
      (map (lambda (path)
             (let loop ((p path)
                        (i 0)
                        (np '()))
               (cond ((null? p) np)
                     (else (loop (cdr p)
                                 (+ i 1)
                                 (cons ((if (even? i) rnode->key redge->key) (car p) kgraph)
                                       np))))))
           rgraph-paths))

    (make-summary-fragment
      (normalize-paths rgraph-paths kgraph)
      (map (lambda (rnode)
             (summarize-rnode rnode kgraph))
           (rgraph-nodes rgraph))
      (map (lambda (redge)
             (summarize-redge redge kgraph))
           (rgraph-edges rgraph))))

  (map (lambda (answer)
         (let* ((reporting-agent (answer-agent   answer))
                (trapi-message   (answer-message answer))
                (trapi-results   (jsexpr-object-ref trapi-message 'results))
                (kgraph          (jsexpr-object-ref trapi-message 'knowledge_graph)))
           (make-condensed-summary
             reporting-agent
             (foldl (lambda (trapi-result summary-fragment)
                     (merge-summary-attrs
                       summary-fragment
                       (trapi-result->summary-fragment trapi-result kgraph)))
                   empty-summary-fragment
                   trapi-results))))
       answers))

(define (condensed-summaries->summary qid condensed-summaries)
  (define (fragment-paths->results/paths fragment-paths)
    (let loop ((results '())
               (paths   '())
               (fps fragment-paths))
      (cond ((null? fps)
              (values results paths))
            (else
              (let* ((fp (car fps))
                     (path-key (path->key fp)))
                (loop (cons `(,(car fp) . ,path-key) results)
                      (cons `(,path-key . ,fp)       paths)
                      (cdr fps)))))))

  (define (extend-summary-results results new-results)
    (let loop ((results results)
               (new-results new-results))
      (cond ((null? new-results)
               results)
            (else
              (let* ((nr (car new-results))
                     (nr-drug (car nr))
                     (nr-path (cdr nr))
                     (rs (jsexpr-object-ref results nr-drug (jsexpr-object)))
                     (paths (jsexpr-object-ref rs 'paths (jsexpr-array))))
                (loop (jsexpr-object-set
                        results
                        nr-drug
                        (jsexpr-object-set rs 'paths (jsexpr-array-prepend paths (symbol->string nr-path))))
                      (cdr new-results)))))))

  (define (extend-summary-paths paths new-paths agent)
    (define (make-path-object path agent)
      (make-jsexpr-object `((subgraph . ,(map symbol->string path))
                            (aras     . ,(list agent)))))

    (foldl
      (lambda (new-path paths)
        (if (null? new-path)
          paths
          (let* ((path-key (car new-path))
                 (path (cdr new-path))
                 (p (jsexpr-object-ref paths path-key)))
            (jsexpr-object-set
              paths
              path-key
              (if p
                (jsexpr-object-transform p 'aras (lambda (aras) (jsexpr-array-prepend aras agent)))
                (make-path-object path agent))))))
           paths
           new-paths))

  (define (extend-summary-obj objs updates agent)
    (let loop ((updates updates)
               (objs objs))
      (cond ((null? updates)
              objs)
            (else
              (loop (cdr updates)
                    (let* ((update (car updates))
                           (up-key (car update))
                           (ups (cdr update)))
                      (jsexpr-object-transform
                        objs
                        up-key
                        (lambda (obj)
                          (foldl (lambda (up obj)
                                   (jsexpr-object-transform
                                     (up obj)
                                     'aras
                                     (lambda (obj)
                                       (jsexpr-array-prepend obj agent))
                                     (jsexpr-array)))
                                 obj
                                 ups))
                        (jsexpr-object))))))))

  (define (extend-summary-nodes nodes node-updates agent)
    (extend-summary-obj nodes node-updates agent))

  (define (extend-summary-edges edges edge-updates agent)
    (extend-summary-obj edges edge-updates agent))

  (define (extend-summary-publications publications edge)
    (define snippets (jsexpr-object-ref edge 'snippets))
    (let loop ((publication-ids (jsexpr-object-ref edge 'publications '()))
                (publications publications))
      (cond ((null? publication-ids)
              publications)
            (else
              (loop (cdr publication-ids)
                    (let ((kvp (assoc (string->symbol (car publication-ids)) snippets)))
                      (if kvp
                        (let* ((pub-id (car kvp))
                               (publication-object (cdr kvp))
                               (snippet (jsexpr-object-ref publication-object 'sentence))
                               (pubdate (jsexpr-object-ref publication-object '|publication date|)))
                        (jsexpr-object-set
                          publications
                          pub-id
                          (hash 'url (id->url (symbol->string pub-id))
                                'snippet snippet
                                'pubdate pubdate)))
                        publications)))))))

  (define (edges->edges/publications edges)
    (values
      (jsexpr-object-map edges (lambda (edge) (jsexpr-object-remove edge 'snippets)))
      (let loop ((edges (jsexpr-object-values edges))
                 (publications (hash)))
        (cond ((null? edges)
                publications)
              (else
                (loop (cdr edges)
                      (extend-summary-publications publications (car edges))))))))

  (define (expand-results results paths nodes)
    (map (lambda (result)
           (let* ((ps (jsexpr-object-ref result 'paths))
                  (subgraph (jsexpr-object-ref-recursive
                              paths
                              `(,(string->symbol (car ps))
                                 subgraph)))
                  (drug (first subgraph))
                  (drug-name (jsexpr-object-ref-recursive
                               nodes
                               `(,(string->symbol drug) names)))
                  (disease (last subgraph)))
             (jsexpr-object-multi-set result `((subject   . ,drug)
                                               (drug_name . ,(if (null? drug-name)
                                                               'null
                                                               (car drug-name)))
                                               (object    . ,disease)))))
         results))

  (define (jsexpr-object-remove-duplicates obj)
    (jsexpr-object-map
      obj
      (lambda (attr)
        (if (list? attr)
          (remove-duplicates attr)
          attr))))

  (let loop ((results (jsexpr-object))
             (paths   (jsexpr-object))
             (nodes   (jsexpr-object))
             (edges   (jsexpr-object))
             (css     condensed-summaries))
    (cond ((null? css)
            (let-values (((edges publications) (edges->edges/publications (jsexpr-object-map
                                                                            edges
                                                                            jsexpr-object-remove-duplicates))))
              (make-jsexpr-object
                `((meta         . ,(metadata-object qid (map condensed-summary-agent condensed-summaries)))
                  (results      . ,(expand-results
                                     (map jsexpr-object-remove-duplicates
                                         (jsexpr-object-values results))
                                     paths
                                     nodes))
                  (paths        . ,(jsexpr-object-map paths jsexpr-object-remove-duplicates))
                  (nodes        . ,(jsexpr-object-map nodes jsexpr-object-remove-duplicates))
                  (edges        . ,edges)
                  (publications . ,publications)))))
          (else
            (define cs (car css))
            (define agent (condensed-summary-agent cs))
            (define-values (new-results new-paths)
              (fragment-paths->results/paths (condensed-summary-paths cs)))
            (loop (extend-summary-results results new-results)
                  (extend-summary-paths paths new-paths agent)
                  (extend-summary-nodes nodes (condensed-summary-nodes cs) agent)
                  (extend-summary-edges edges (condensed-summary-edges cs) agent)
                  (cdr css))))))

(define (trapi-answers->summary trapi-answers primary-predicates static-node-rules
    variable-node-rules edge-rules)
  (define (make-static-node slot ids)
    (and ids (not (jsexpr-null? ids)) (cons slot (list->set ids))))
  (define (static-node-slot node) (car node))
  (define (find-valid-qedge qedges)
    (let loop ((qes (jsexpr-object->alist qedges))
               (res #f))
      (if (or res (null? qes))
          res
          (let ((qe (cdar qes)))
            (loop (cdr qes)
                  (if (ormap (lambda (p) (member p primary-predicates))
                             (jsexpr-object-ref qe 'predicates))
                      qe
                      res))))))
  (define (edge-valid? static-node kedge)
    (set-member? (cdr static-node) (jsexpr-object-ref kedge (car static-node))))
  (define (trapi-message-valid? trapi-message)
    (let ((qg (jsexpr-object-ref trapi-message 'query_graph)))
      (and qg
           (find-valid-qedge (jsexpr-object-ref qg 'edges))
           (jsexpr-object-has-key? trapi-message 'knowledge_graph)
           (< (jsexpr-object-count (jsexpr-object-ref qg 'nodes))))))
  (define (update-result-summary summary edge-summary)
    (define result-id (car edge-summary))
    (list (if result-id result-id (car summary))
          (append (cadr summary) (cadr edge-summary))
          (append (caddr summary) (caddr edge-summary))
          (append (cadddr summary) (cadddr edge-summary))))

  (define (update-summary summary result-summary var-slot)
    (define result-id (car result-summary)) ; Must have a result ID at this point to be valid
    (define static-summary (car summary))
    (define var-summary (cdr summary))
    (define (update updates)
      (let loop ((ups updates)
                 (result '()))
        (if (null? ups)
            result
            (let ((updaters (caar ups))
                  (obj      (cdar ups)))
              (loop (cdr ups)
                    (append result
                            (if (null? updaters)
                                `(,updaters ,obj)
                                `(,(cdr updaters) ,((car updaters) obj)))))))))
    (if result-id
      (let ((rs (if (jsexpr-object-has-key? var-summary result-id)
                    (jsexpr-object-ref var-summary result-id)
                    (hash var-slot (hash)
                          'edge    (hash)))))
        (let loop ((static-updaters (cadr result-summary))
                   (sn              static-summary)
                   (var-updaters    (caddr result-summary))
                   (vns             (jsexpr-object-ref rs var-slot))
                   (edge-updaters   (cadddr result-summary))
                   (es              (jsexpr-object-ref rs 'edge)))
          (if (and (null? static-updaters) (null? var-updaters) (null? edge-updaters))
              (let ((result (hash-set (hash-set rs var-slot vns) 'edge es)))
                (cons sn (hash-set var-summary result-id result)))
              (apply loop (update `((,static-updaters . ,sn)
                                    (,var-updaters    . ,vns)
                                    (,edge-updaters   . ,es)))))))
      summary))

  (define (summarize-edge edge-binding static-node var-slot knowledge-graph)
    (define kedge (trapi-edge-binding->trapi-kedge knowledge-graph edge-binding))
    (if (edge-valid? static-node kedge) ; Valid if the predicate matches the direction in the qgraph
        (let ((var-binding (string->symbol (jsexpr-object-ref kedge var-slot)))
              (static-binding (string->symbol (jsexpr-object-ref kedge (static-node-slot static-node)))))
          (list var-binding
                (static-node-rules (trapi-node-binding->trapi-knode knowledge-graph static-binding))
                (variable-node-rules (trapi-node-binding->trapi-knode knowledge-graph var-binding))
                (edge-rules kedge)))
        (list #f '() '() '())))

  (define (summarize-result result static-node var-slot knowledge-graph)
    (let loop ((edge-bindings (map (lambda (b)
                                     (string->symbol (jsexpr-object-ref (car b) 'id)))
                                     (jsexpr-object-values (jsexpr-object-ref result 'edge_bindings))))
               (summary (list #f '() '() '())))
      (cond ((null? edge-bindings) summary)
            (else
              (define edge-summary (summarize-edge (car edge-bindings) static-node var-slot knowledge-graph))
              (loop (cdr edge-bindings)
                    (update-result-summary summary edge-summary))))))

  (define (summarize-answer answer summary)
    (define qgraph (jsexpr-object-ref answer 'query_graph))
    (define kgraph (jsexpr-object-ref answer 'knowledge_graph))
    (define qnodes (jsexpr-object-ref qgraph 'nodes))
    (define qedges (jsexpr-object-ref qgraph 'edges))
    (define qedge  (find-valid-qedge qedges))
    (define qobj (string->symbol (jsexpr-object-ref qedge 'object)))
    (define qsub (string->symbol (jsexpr-object-ref qedge 'subject)))
    (define qobj-ids (jsexpr-object-ref-recursive qnodes `(,qobj ids)))
    (define qsub-ids (jsexpr-object-ref-recursive qnodes `(,qsub ids)))
    (define-values (static-slot var-slot)
      (if (and qobj-ids (not (jsexpr-null? qobj-ids)))
          (values 'object 'subject)
          (values 'subject 'object)))
    (define static-node (make-static-node
                          static-slot
                          (if (equal? static-slot 'object)
                              qobj-ids
                              qsub-ids)))
    (if (not static-node)
      summary ; Skipping because there is an edge with only variable nodes
      (let loop ((results (jsexpr-object-ref answer 'results))
                 (summary summary))
        (if (null? results)
          summary
          (loop (cdr results)
                (update-summary
                  summary
                  (summarize-result (car results) static-node var-slot kgraph) var-slot))))))

  (let loop ((answers trapi-answers)
             (summary (cons (jsexpr-object) (jsexpr-object))))
    (if (null? answers)
      summary
      (let ((trapi-message (answer-message (car answers))))
        (loop (cdr answers)
              (if (trapi-message-valid? trapi-message)
                (summarize-answer trapi-message summary)
                summary))))))

(define (answers->summary result expanders)
  (define fda-path '(fda_info highest_fda_approval_status))
  (define primary-predicates (config-primary-predicates SERVER-CONFIG))

  (define summary
    (trapi-answers->summary
      result
      primary-predicates
      (make-summarize-rules ; static node rules
        `(,(aggregate-property 'name '(names))
          ,(aggregate-property 'categories '(types))
          ,(aggregate-attributes
            `(,(biolink-tag "xref"))
            'curies)
        )
      )
      (make-summarize-rules ; variable node rules
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
          ,(aggregate-property-when
            'predicate
            '(secondary_predicates)
            (lambda (p) (not (member p primary-predicates))))
          ,(aggregate-and-transform-attributes
            `(,(biolink-tag "supporting_document")
              ,(biolink-tag "Publication")
              ,(biolink-tag "publications"))
            'evidence
            (lambda (evidence)
              (if (list? evidence)
                  evidence
                  (string-split evidence #rx",|\\|"))))))))

  ; Post processing stuff
  (define (jsexpr-remove-duplicates answer kpaths)
    (let loop ((kps kpaths)
               (a answer))
      (if (null? kps)
          a
          (loop (cdr kps)
                (jsexpr-object-set-recursive a (car kps)
                  (remove-duplicates
                    (jsexpr-object-ref-recursive a (car kps))))))))

  (define (apply-post-processing obj fs)
    (let loop ((fs fs)
               (obj obj))
      (if (null? fs)
          obj
          (loop (cdr fs)
                (map (car fs) obj)))))

  (let ((sns (car summary))
        (vs  (cdr summary)))
    (hash
      'summary (if (hash-empty? vs)
                 'null
                 (apply-post-processing
                   (expand-evidence
                     (map (lambda (answer)
                             (jsexpr-remove-duplicates answer '((edge evidence))))
                           (jsexpr-object-values vs))
                     expanders)
                   `(,add-last-publication-date)))
      'static_node (if (hash-empty? sns)
                     'null
                     (jsexpr-remove-duplicates sns '((names) (curies) (types)))))))

(module+ test
  (require rackunit)
  (define test-qgraph (read-json (open-input-file "test/trapi/qgraph.json")))
  (define test-query-expected (read-json (open-input-file "test/trapi/query-expected.json")))
  (define (mock-curie-searcher name) '("NCBIGene:23221" "MONDO:0033373" "UMLS:C4693903"))
  (check-equal? (qgraph->trapi-query test-qgraph mock-curie-searcher) test-query-expected)

  (define test-invalid-qgraph (read-json (open-input-file "test/trapi/invalid-qgraph.json")))
  (check-false (qgraph->trapi-query test-invalid-qgraph))

  ; If the predicate is not in the correct direction the edge is skipped
  (define summary-skip-edge `(,(cons (read-json (open-input-file "test/trapi/summary-skip-edge.json"))
                                     "test")))
  (check-equal? (answers->summary summary-skip-edge '())
                (hash 'summary 'null
                      'static_node 'null))
)
