#lang racket/base

(require
  racket/string
  racket/match
  racket/list
  net/http-client
  xml
  xml/path
  srfi/19 
  "common.rkt")

(provide
  add-last-publication-date
  expand-evidence)

(define (id->link id)
  (match id
    ((pregexp "^PMC[0-9]+$") (pmcid->pubmed-article-link id))
    ((pregexp "^PMID:")      (pmid->pubmed-link (strip-id-tag id)))
    ((pregexp "^DOI:")       (doiid->doi-link (strip-id-tag id)))
    ((pregexp "^NCT[0-9]+$") (nctid->nct-link id))
    (_ "Unknown supporting document type")))
(define (strip-id-tag id)
  (cadr (string-split id ":")))
(define (tag-pmid id)
  (string-append "PMID:" id))
(define (pmcid->pubmed-article-link id) 
  (string-append "https://www.ncbi.nlm.nih.gov/pmc/articles/" id))
(define (pmid->pubmed-link id)
  (string-append "https://pubmed.ncbi.nlm.nih.gov/" id))
(define (doiid->doi-link id)
  (string-append "https://www.doi.org/" id))
(define (nctid->nct-link id)
  (format "https://clinicaltrials.gov/search?id=%22~a%22" id))

(define (eutils-date->string eutils-date)
  (define (numeric-month? m)
    (string->number m))

  (let ((date-elements (list (se-path* '(Day) eutils-date)
                             (se-path* '(Month) eutils-date)
                             (se-path* '(Year) eutils-date))))
    (match date-elements 
      ((list #f #f #f) 'null)
      ((list _ #f y) y)
      ((list d (? numeric-month? m) y) (string-join (if d date-elements (cdr date-elements)) "/"))
      ((list #f m y) (date->string (string->date (string-join (cdr date-elements)) "~b ~Y") "~m/~Y"))
      ((list d m y) (date->string (string->date (string-join date-elements) "~d ~b ~Y") "~d/~m/~Y"))
      (_ 'null))))

(define (make-eutils-request action params)
  (define eutils-host "eutils.ncbi.nlm.nih.gov")
  (define eutils-uri (format "/entrez/eutils/~a.fcgi~a"
                             action
                             (make-url-params params)))
  (match-define-values (_ _ resp-in)
    (http-sendrecv eutils-host
                  eutils-uri
                  #:ssl? #t
                  #:method #"GET"))
  (xml->xexpr (document-element (read-xml resp-in))))

(define (pubmed-fetch pmids)
  (make-eutils-request "efetch" `(("db" . "pubmed")
                                  ("id" . ,(string-join pmids ","))
                                  ("retmode" . "xml")
                                  ("version" . "2.0"))))

(define (expand-pmid-evidence pmids)
  (define (update-evidence evidence key attrs)
    (jsexpr-object-set evidence key (make-immutable-hash attrs)))
  (define (parse-abstract abstract-elements)
    (if (null? abstract-elements)
        'null
        (string-join
          (foldl (lambda (abstract-fragment acc)
                   (cons (match abstract-fragment
                           ((? string?) abstract-fragment)
                           ((list _ _ text) text)
                           (_ abstract-fragment))
                         acc))
                 '() abstract-elements))))

  (define untagged-ids (map (lambda (pmid) (cadr (string-split pmid ":")))
                       pmids))
  (define pubmed-articles (se-path*/list '(PubmedArticleSet)
                                         (pubmed-fetch untagged-ids)))
  (let loop ((articles pubmed-articles)
             (expanded-evidence (jsexpr-object)))
    (if (null? articles)
        expanded-evidence
        (let* ((a (car articles))
               (pmid (se-path* '(PMID) a))
               (title (se-path* '(ArticleTitle) a))
               (pubdate `(PubDate ,@(se-path*/list '(PubDate) a)))
               (abstract-elements (se-path*/list '(AbstractText) a)))
          (loop (cdr articles)
                (if (null? pmid)
                    expanded-evidence
                    (let ((tagged-pmid (tag-pmid pmid)))
                      (jsexpr-object-set expanded-evidence
                                          (string->symbol tagged-pmid)
                                          (make-immutable-hash
                                          `((url . ,(id->link tagged-pmid))
                                            (title . ,(or title 'null))
                                            (pubdate . ,(eutils-date->string pubdate))
                                            (abstract . ,(parse-abstract abstract-elements))))))))))))

  (define (expand-evidence answer)
    (define evidence-ids
      (foldl (lambda (a ids)
               (append ids 
                 (filter (lambda (id) (regexp-match (pregexp "^PMID:") id))
                         (remove-duplicates (jsexpr-object-ref-recursive a
                                                                         '(edge evidence) 
                                                                         '())))))
             '()
             answer))
    (define test-evidence-ids (take evidence-ids (min 400 (length evidence-ids))))
    (define expanded-evidence (expand-pmid-evidence test-evidence-ids))
    (map (lambda (a)
           (let loop ((edge-evidence (jsexpr-object-ref-recursive a '(edge evidence) '()))
                      (expanded-edge-evidence '()))
             (if (null? edge-evidence)
                 (jsexpr-object-set-recursive a '(edge evidence) expanded-edge-evidence)
                 (loop (cdr edge-evidence)
                       (let* ((e (car edge-evidence))
                              (ee (jsexpr-object-ref expanded-evidence (string->symbol e))))
                         (if ee (cons ee expanded-edge-evidence) expanded-edge-evidence))))))
         answer))

  (define (add-last-publication-date answer)
    (define (pad-date d)
      (let loop ((d d)
                 (l (length d)))
        (if (equal? (length d) 3)
            d
            (loop (cons "00" d)
                  (+ l 1)))))
    (define (date>? a b)
      (let* ((aes (string-split a "/"))
             (bes (string-split b "/"))
             (res (foldl (lambda (ae be cmp)
                           (cond ((not (equal? cmp '=)) cmp)
                                 ((string>? ae be) '>)
                                 ((string<? ae be) '<)
                                 (else '=)))
                         '=
                         (reverse (pad-date aes))
                         (reverse (pad-date bes)))))
        (cond ((equal? res '<) #f)
              ((equal? res '>) #t)
              (else (> (length aes) (length bes)))))) ; If all else is equal choose the more specific one

    (define publication-dates (filter (lambda (e) (not (jsexpr-null? e)))
                                      (map (lambda (e) (jsexpr-object-ref e 'pubdate))
                                           (jsexpr-object-ref-recursive answer '(edge evidence)))))
    (jsexpr-object-set-recursive answer
                                 '(edge last_publication_date)
                                 (if (null? publication-dates)
                                     'null
                                     (car (sort publication-dates date>?)))))