#lang racket/base

(require
  racket/string
  racket/match
  racket/list
  net/http-client
  xml
  xml/path
  srfi/19 
  "common.rkt"
  "config.rkt"
  
  racket/pretty)

(provide
  add-last-publication-date
  expand-evidence)

(define id->link (config-id->url SERVER-CONFIG))
(define id-patterns (config-id-patterns SERVER-CONFIG))
(define (tag-pmid id)
  (string-append "PMID:" id))
(define (pmcid->pubmed-article-link id) 
  (string-append "https://www.ncbi.nlm.nih.gov/pmc/articles/" id))
(define (doiid->doi-link id)
  (string-append "https://www.doi.org/" id))

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

(define (make-eutils-request action params (data #f))
  (define eutils-endpoint (config-eutils-endpoint SERVER-CONFIG))
  (define eutils-host   (host eutils-endpoint))
  (define eutils-uri    (uri  eutils-endpoint))
  (define eutils-params (make-url-params params))
  (match-define-values (_ _ resp-in)
    (http-sendrecv eutils-host
                   (string-append eutils-uri eutils-params) 
                   #:ssl? #t
                   #:method (if data #"POST" "#GET")
                   #:data data))
  (xml->xexpr (document-element (read-xml resp-in))))

(define (pubmed-fetch pmids)
  (make-eutils-request "efetch" '(("db" . "pubmed")
                                  ("retmode" . "xml")
                                  ("version" . "2.0"))
                                  (string->bytes/utf-8 (format "id=~a" (string-join pmids ",")))))

(define (expand-pmid-evidence pmids expanded-evidence)
  (define (update-evidence evidence key attrs)
    (jsexpr-object-set evidence key (make-immutable-hash attrs)))
  (define (parse-fragment abstract-fragment)
    (match abstract-fragment
      ((? string?) abstract-fragment)
      ((list _ _ fragment) (parse-fragment fragment))
      ((list _ _ (and strs (? string?)) ...) (string-join strs))
      (_ (pretty-print (format "Warning: skipping abstract fragment ~a" abstract-fragment))
         "")))
  (define (parse-abstract abstract-elements)
    (if (null? abstract-elements)
        'null
        (string-join
          (reverse
          (foldl (lambda (abstract-fragment acc)
                     (cons (parse-fragment abstract-fragment) acc))
                   '() abstract-elements)))))

  (define untagged-ids (map (lambda (pmid) (cadr (string-split pmid ":")))
                       pmids))
  (define pubmed-articles (se-path*/list '(PubmedArticleSet)
                                         (pubmed-fetch untagged-ids)))
  (let loop ((articles pubmed-articles)
             (expanded-evidence expanded-evidence))
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

(define (expand-nct-evidence nctids expanded-evidence)
  (let loop ((ids nctids)
             (expanded-evidence expanded-evidence))
    (if (null? ids)
        expanded-evidence
        (loop (cdr ids)
              (jsexpr-object-set expanded-evidence
                                 (string->symbol (car ids))
                                 (make-immutable-hash
                                   `((url . ,(id->link (car ids)))
                                     (pubdate . null))))))))

(define (expand-evidence answers)
  (define (id->equiv-class id)
    (let loop ((ps id-patterns)
                (i 0))
      (if (regexp-match? (pregexp (car ps)) id)
          i
          (loop (cdr ps) (+ i 1)))))
  (define (get-all-valid-ids answers)
      (remove-duplicates
        (foldl (lambda (a ids)
                (append ids 
                (filter (lambda (id)
                          (let loop ((ps id-patterns))
                            (cond ((null? ps) #f)
                                  ((regexp-match? (pregexp (car ps)) id) #t)
                                  (else (loop (cdr ps))))))
                          (jsexpr-object-ref-recursive a '(edge evidence) '()))))
              '()
            answers)))
  (define evidence-ids (group-by id->equiv-class (get-all-valid-ids answers)))
  (define expanded-evidence (foldl (lambda (id-expander ids evidence)
                                     (id-expander ids evidence))
                                 (jsexpr-object)
                                 (list expand-pmid-evidence expand-nct-evidence) ; This relies on ordering in config file :(
                                 evidence-ids))
    (map (lambda (a)
           (let loop ((edge-evidence (jsexpr-object-ref-recursive a '(edge evidence) '()))
                      (expanded-edge-evidence '()))
             (if (null? edge-evidence)
                 (jsexpr-object-set-recursive a '(edge evidence) expanded-edge-evidence)
                 (loop (cdr edge-evidence)
                       (let* ((e (car edge-evidence))
                              (ee (jsexpr-object-ref expanded-evidence (string->symbol e))))
                         (if ee (cons ee expanded-edge-evidence) expanded-edge-evidence))))))
        answers))

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