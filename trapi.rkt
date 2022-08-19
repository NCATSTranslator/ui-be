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
  (prefix-in bl: "biolink-model.rkt")

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

(define (aggregate-property-update-when v obj kpath update?)
  (define cv (jsexpr-object-ref-recursive obj kpath))
  (cond ((update? v)
         (jsexpr-object-set-recursive
           obj
           kpath
           (append (if cv cv '()) (if (list? v) v (list v)))))
        (cv obj)
        (else (jsexpr-object-set-recursive obj kpath '()))))
(define (aggregate-property-update v obj kpath)
  (aggregate-property-update-when v obj kpath (lambda (_) #t)))

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
      (aggregate-property-update-when v obj kpath update?))
    '()))
(define (aggregate-and-transform-property src-key kpath transform)
  (make-mapping
    src-key
    transform
    (lambda (v obj)
      (aggregate-property-update v obj kpath))
    '()))
(define (aggregate-property src-key kpath)
  (aggregate-property-when src-key kpath (lambda (_) #t)))

(define (rename-and-transform-attribute attribute-id kpath transform)
  (make-mapping
    'attributes
    (lambda (attributes)
      (let loop ((as attributes)
                 (result #f))
        (cond (result
                (transform result))
              ((or (jsexpr-null? as) (jsexpr-array-empty? as))
                'null)
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
        (cond ((or (jsexpr-null? as) (jsexpr-array-empty? as))
                result)
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
(define (make-rgraph nodes edges kgraph)
  (cons nodes
        (filter (lambda (e)
                  (define kedge (trapi-edge-binding->trapi-kedge kgraph e))
                  (bl:biolink-predicate? (jsexpr-object-ref kedge 'predicate)))
                edges)))
(define (rgraph-nodes rgraph) (car rgraph))
(define (rgraph-edges rgraph) (cdr rgraph))
(define (redge-inverted? redge subject kgraph)
  (let ((kedge (trapi-edge-binding->trapi-kedge kgraph redge)))
    (equal? subject (jsexpr-object-ref kedge 'object))))

(define (trapi-result->rgraph trapi-result kgraph)
  (make-rgraph (flatten-bindings (jsexpr-object-ref trapi-result 'node_bindings))
               (flatten-bindings (jsexpr-object-ref trapi-result 'edge_bindings))
               kgraph))
(define (rnode->key rnode kgraph) rnode)
; We treat redges as bi-directional but the key is not. Generate the key for the
; invert of the original kedge using invert?
(define (redge->key redge kgraph (invert? #f))
  (let* ((kedge (trapi-edge-binding->trapi-kedge kgraph redge))
         (ksubject   (jsexpr-object-ref kedge 'subject))
         (kpredicate (jsexpr-object-ref kedge 'predicate))
         (kobject    (jsexpr-object-ref kedge 'object)))
    (path->key
      (if invert?
        (list kobject (bl:invert-biolink-predicate kpredicate) ksubject)
        (list ksubject kpredicate kobject)))))

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
        (match-let* ((`(,redge . ,rest) edges)
                     (`(,subject . ,object)
                       (redge->edge-id redge)))
        (loop rest
              (hash-set
                (hash-set
                  res
                  subject
                  (cons (cons redge object) (hash-ref res subject '())))
                object
                (cons (cons redge subject) (hash-ref res object '()))))))))
  (lambda (rnode)
    ; By defaulting to an empty list we are saying to ignore all paths that are not the
    ; terminal node and where the node does not appear in any edge where the node is the
    ; subject.
    (hash-ref rnode->out-edges rnode '())))

(define (rgraph-fold proc init acc)
  (let loop ((objs-left init)
             (res acc))
    (if (null? objs-left)
      res
      (match-let* ((`(,new-obj-left . ,new-res)
                     (proc (car objs-left))))
        (loop (append new-obj-left (cdr objs-left))
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
      `(,(aggregate-and-transform-property
           'predicate
           '(predicates)
           bl:sanitize-predicate)
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

  (define max-hops (config-max-hops SERVER-CONFIG))

  (condensed-summaries->summary
    qid
    (creative-answers->condensed-summaries
      answers
      node-rules
      edge-rules
      max-hops)))

(define (creative-answers->condensed-summaries answers node-rules edge-rules max-hops)
  (define (trapi-result->summary-fragment trapi-result kgraph)
    (define node-bindings (jsexpr-object-ref trapi-result 'node_bindings))
    (define rgraph (trapi-result->rgraph trapi-result kgraph))
    (define drug    (get-binding-id node-bindings 'drug))
    (define disease (get-binding-id node-bindings 'disease))
    (define rnode->out-edges (make-rnode->out-edges rgraph kgraph))
    (define max-path-length (+ (* 2 max-hops) 1))
    (define rgraph-paths
      (rgraph-fold (lambda (path)
                     (let ((current-rnode (car path)))
                       (cond
                         ((< max-path-length (length path))
                           (cons '() '())) ; Skip this path if its too long
                         ((equal? current-rnode disease)
                           (cons '() `(,path)))
                         (else
                           (cons (filter (lambda (p) p)
                                         (map (match-lambda
                                                ((cons next-edge next-node)
                                                 (and (not (member next-node path)) ; No cycles
                                                      (cons next-node (cons next-edge path)))))
                                              (rnode->out-edges current-rnode)))
                                 '())))))
                   `((,drug))
                   '()))

    ; Return a pair (key, update) for a node
    (define (summarize-rnode rnode kgraph)
      (cons (rnode->key rnode kgraph) (node-rules (trapi-node-binding->trapi-knode kgraph rnode))))

    ; Return a the list (key, inverted-key, update) for an edge
    ; We must generate the inverted-key now before edges are merged otherwise its possible
    ; Our edge keys won't match with the path key
    (define (summarize-redge redge kgraph)
      (let ((kedge (trapi-edge-binding->trapi-kedge kgraph redge)))
        (list (redge->key redge kgraph)
              (redge->key redge kgraph #t)
              (edge-rules kedge))))

    ; Convert paths structure to normalized node and edge IDs
    (define (normalize-paths rgraph-paths kgraph)
      (define (N n) (rnode->key n kgraph))
      (define (E e s) (redge->key e kgraph (redge-inverted? e s kgraph)))
      (map (lambda (path)
             (let loop ((p path)
                        (np '()))
               (match p
                 ('()           np)
                 (`(,n)         (cons (N n) np))
                 (`(,n ,e . ,p) (loop p (cons (E e n) (cons (N n) np)))))))
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
              (match-let* ((`(,fp . ,rest) fps)
                           ((? symbol? path-key) (path->key fp)))
                (loop (cons `(,(car fp) . ,path-key) results)
                      (cons `(,path-key . ,fp)       paths)
                      rest))))))

  (define (extend-summary-results results new-results)
    (let loop ((results results)
               (new-results new-results))
      (cond ((null? new-results)
               results)
            (else
              (match-let* ((`(,nr . ,rest)         new-results)
                           (`(,nr-drug . ,nr-path) nr)
                           ((? jsexpr-object? rs)
                            (jsexpr-object-ref results nr-drug (jsexpr-object)))
                           ((? jsexpr-array? paths)
                            (jsexpr-object-ref rs 'paths (jsexpr-array))))
                (loop (jsexpr-object-set
                        results
                        nr-drug
                        (jsexpr-object-set
                          rs
                          'paths
                          (jsexpr-array-prepend paths (symbol->string nr-path))))
                      rest))))))

  (define (extend-summary-paths paths new-paths agent)
    (define (make-path-object path agent)
      (make-jsexpr-object `((subgraph . ,(map symbol->string path))
                            (aras     . ,(list agent)))))

    (foldl
      (lambda (new-path paths)
        (if (null? new-path)
          paths
          (match-let* ((`(,path-key . ,path) new-path)
                       ((and p) (jsexpr-object-ref paths path-key)))
            (jsexpr-object-set
              paths
              path-key
              (if p
                (jsexpr-object-transform p 'aras (lambda (aras) (jsexpr-array-prepend aras agent)))
                (make-path-object path agent))))))
           paths
           new-paths))

  (define (extend-summary-objs objs key updates agent)
    (jsexpr-object-transform
      objs
      key
      (lambda (obj)
        (foldl (lambda (up obj)
                 (jsexpr-object-transform
                   (up obj)
                   'aras
                   (lambda (obj)
                     (jsexpr-array-prepend obj agent))
                   (jsexpr-array)))
               obj
               updates))
      (jsexpr-object)))

  (define (extend-summary-nodes nodes node-updates agent)
    (let loop ((updates node-updates)
               (nodes nodes))
      (cond ((null? updates)
             nodes)
            (else
              (loop (cdr updates)
                    (match-let* ((`(,update . , rest) updates)
                                 (`(,k . ,ups)        update))
                      (extend-summary-objs nodes k ups agent)))))))

  (define (extend-summary-edges edges edge-updates agent)
    (let loop ((updates edge-updates)
               (edges edges))
      (cond ((null? updates)
              edges)
            (else
              (loop (cdr updates)
                    (match-let* ((`(,update . ,rest) updates)
                                 (`(,k ,ik ,ups)     update))
                      (jsexpr-object-transform
                        (extend-summary-objs edges k ups agent)
                        k
                        (lambda (edge)
                          (jsexpr-object-set edge 'invert-key ik)))))))))

  (define (extend-summary-publications publications edge)
    (define snippets (jsexpr-object-ref edge 'snippets))
    (define (make-publication-object url snippet pubdate)
      (hash 'url url
            'snippet snippet
            'pubdate pubdate))

    (let loop ((publication-ids (jsexpr-object-ref edge 'publications '()))
               (publications publications))
      (cond ((null? publication-ids)
             publications)
            (else
              (match-let* ((`(,pub-id-str . ,rest) publication-ids)
                           ((? symbol? pub-id)
                            (string->symbol pub-id-str))
                           ((? string? url)
                            (id->url pub-id-str)))
                (loop rest
                      (let ((kvp (assoc pub-id snippets)))
                        (jsexpr-object-set
                          publications
                          pub-id
                          (if kvp
                            (match-let* ((`(,pub-id . ,publication-object) kvp)
                                         ((? jsexpr-string? snippet)
                                          (jsexpr-object-ref publication-object 'sentence))
                                         ((? jsexpr-string? pubdate)
                                          (jsexpr-object-ref publication-object '|publication date|)))
                              (make-publication-object url snippet pubdate))
                            (make-publication-object url 'null 'null))))))))))

  (define (edges->edges/publications edges)
    (define (invert-edge edge)
      (cons (jsexpr-object-ref edge 'invert-key)
            (jsexpr-object-transform
              edge
              'predicates
              (lambda (predicates)
                (jsexpr-array-map bl:invert-biolink-predicate predicates)))))

    (let loop ((es (jsexpr-object-values edges))
               (final-edges edges)
               (publications (hash)))
      (cond ((null? es)
             (values (jsexpr-object-map
                       final-edges
                       (lambda (e)
                         (jsexpr-object-multi-remove e '(snippets invert-key))))
                     publications))
            (else
              (match-let* ((`(,e . ,rest) es)
                           (`(,ik . ,ie) (invert-edge e)))
                (loop rest
                      (if (jsexpr-object-has-key? final-edges ik)
                        (begin
                          (pretty-display "NOOOO")
                          final-edges)
                        (jsexpr-object-set final-edges ik ie))
                      (extend-summary-publications publications e)))))))

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
           (let-values (((edges publications)
                         (edges->edges/publications
                           (jsexpr-object-map
                             edges
                             (lambda (edge)
                               (jsexpr-object-key-map
                                 (jsexpr-object-remove-duplicates edge)
                                 '(publications)
                                 (lambda (publications)
                                   (filter valid-id? publications))))))))
             (make-jsexpr-object
               `((meta         . ,(metadata-object
                                    qid
                                    (map condensed-summary-agent condensed-summaries)))
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
