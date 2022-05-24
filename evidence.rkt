#lang racket/base

(require
  racket/string
  racket/match
  racket/list
  net/http-client
  xml
  srfi/19
  json
  "common.rkt"

  racket/pretty)

(provide
  add-last-publication-date
  expand-evidence
  make-pmid-expander
  make-nct-expander)

(define (make-expander id-pattern
                       ids->data
                       data-parser)
  (list id-pattern ids->data data-parser))
(define (id-pattern expander) (car expander))
(define (ids->data expander) (cadr expander))
(define (data-parser expander) (caddr expander))

(define (make-nct-expander (fetch-method nct-fetch))
  (make-expander "^NCT[0-9]*"
                 fetch-method
                 nct-data-parser))

(define (make-pmid-expander (fetch-method pubmed-fetch))
  (make-expander "^PMID:"
                 fetch-method
                 pubmed-data-parser))

(define (pmcid->pubmed-article-link id)
  (string-append "https://www.ncbi.nlm.nih.gov/pmc/articles/" id))
(define (doiid->doi-link id)
  (string-append "https://www.doi.org/" id))

(define (date-elements->string date-elements)
  (define (numeric-month? m)
    (string->number m))
  (define (abbrv-month? m)
    (member m '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))

  (if (or (null? date-elements) (not (last date-elements)))
      'null
      (let ((padded-date (map (lambda (e) (if e e "01"))
                              date-elements)))
        (date->string
          (match padded-date
            ((list d (? numeric-month? m) y) (string->date (string-join padded-date) "~d ~m ~Y"))
            ((list d (? abbrv-month?   m) y) (string->date (string-join padded-date) "~d ~b ~Y"))
            ((list d m y) (string->date (string-join padded-date) "~d ~B ~Y")))
          "~d/~m/~Y"))))

; Expander procedures for PubMed evidence
(define (make-eutils-request action params (data #f))
  (define eutils-host "eutils.ncbi.nlm.nih.gov")
  (define eutils-uri  "/entrez/eutils/efetch.fcgi")
  (define eutils-params (make-url-params params))
  (match-define-values (_ _ resp-in)
    (http-sendrecv eutils-host
                   (string-append eutils-uri eutils-params)
                   #:ssl? #t
                   #:method (if data #"POST" "#GET")
                   #:data data))
  (xml->xexpr (document-element (read-xml resp-in))))

(define (pubmed-fetch pmids)
  (displayln (format "Processing ~a PMIDs" (length pmids)))
  (define untagged-ids (map (lambda (pmid) (cadr (string-split pmid ":")))
                       pmids))
  (make-eutils-request "efetch" '(("db" . "pubmed")
                                  ("retmode" . "xml")
                                  ("version" . "2.0"))
                                  (string->bytes/utf-8 (format "id=~a" (string-join untagged-ids ",")))))

(define (pubmed-data-parser article-set expanded-evidence)
  (define (update-evidence evidence key attrs)
    (jsexpr-object-set evidence key (make-jsexpr-object attrs)))
  (define (parse-date pubmed-date)
    (let ((date-elements (list (tag->xexpr-value pubmed-date 'Day)
                               (tag->xexpr-value pubmed-date 'Month)
                               (tag->xexpr-value pubmed-date 'Year))))
      (date-elements->string date-elements)))
  (define (id->link id)
    (format "https://pubmed.ncbi.nlm.nih.gov/~a" id))
  (define (tag-pmid id)
    (string-append "PMID:" id))
  (define (parse-abstract abstract-fragments)
    (define (parse-fragment abstract-fragment)
      (match abstract-fragment
        ((? string?) abstract-fragment)
        ((list _ _ fragment) (parse-fragment fragment))
        ((list _ _ (and strs (? string?)) ...) (string-join strs))
        (_ (pretty-print (format "Warning: skipping abstract fragment ~a" abstract-fragment))
          "")))

    (if (null? abstract-fragments)
        'null
        (string-join (map parse-fragment abstract-fragments))))
  (define (parse-journal journal-xexpr)
    (let ((title  (tag->xexpr-value journal-xexpr 'Title))
          (volume `("Volume" . ,(tag->xexpr-value journal-xexpr 'Volume)))
          (issue  `("Issue"  . ,(tag->xexpr-value journal-xexpr 'Issue))))
      (if (not title)
          'null
          (string-join
            `(,title ,@(filter (lambda (str) str)
                               (map (lambda (journal-fragment)
                                       (let ((jfv (cdr journal-fragment)))
                                         (and jfv (string-join (list (car journal-fragment) jfv)))))
                                    (list volume issue))))
                       ", "))))

  (define pubmed-articles (tag->xexpr-fragments article-set 'PubmedArticleSet))
  (let loop ((articles pubmed-articles)
             (expanded-evidence expanded-evidence))
    (if (null? articles)
        expanded-evidence
        (let* ((a (car articles))
               (pmid               (tag->xexpr-value     a 'PMID))
               (title              (tag->xexpr-value     a 'ArticleTitle))
               (pubdate-xexpr      (tag->xexpr-subtree   a 'PubDate))
               (journal-xexpr      (tag->xexpr-subtree   a 'Journal))
               (abstract-fragments (tag->xexpr-fragments a 'AbstractText)))
          (loop (cdr articles)
                (if (null? pmid)
                    expanded-evidence
                    (let ((tagged-pmid (tag-pmid pmid)))
                      (jsexpr-object-set expanded-evidence
                                          (string->symbol tagged-pmid)
                                          (make-jsexpr-object
                                          `((type    . "publication")
                                            (url     . ,(id->link pmid))
                                            (title   . ,(or title 'null))
                                            (dates   . ,(list (parse-date pubdate-xexpr)))
                                            (summary . ,(parse-abstract abstract-fragments))
                                            (source  . ,(parse-journal journal-xexpr))))))))))))

; Expander procedures for NCT evidence
(define nct-fields '("NCTId"
                     "BriefTitle"
                     "StartDate"
                     "CompletionDate"
                     "BriefSummary"
                     "LocationFacility"
                     "LocationCountry"))

(define (nct-fetch nctids)
  (displayln (format "Processing ~a NCT IDs" (length nctids)))
  (define nct-host "clinicaltrials.gov")
  (define nct-uri  "/api/query/study_fields")
  (define nct-params (make-url-params `(("expr"   . ,(string-join nctids "+OR+"))
                                        ("fields" . ,(string-join nct-fields "%2C"))
                                        ("fmt"    . "json"))))
  (match-define-values (_ _ resp-in)
    (http-sendrecv nct-host
                   (string-append nct-uri nct-params)
                   #:ssl? #t
                   #:method #"GET"))

  (with-handlers ((exn:fail:read?
                    (lambda (ex)
                      (pretty-display "Warning: response from nct-fetch is not valid JSON")
                      (pretty-display ex)
                      (jsexpr-object))))
    (read-json resp-in)))

(define (nct-data-parser trial-data expanded-evidence)
  (define (id->link id)
    (format "https://clinicaltrials.gov/search?id=%22~a%22" id))
  (define (parse-date date)
    (if (jsexpr-null? date)
        'null
        (let ((date-elements (map (lambda (s) (string-trim s ","))
                                  (string-split date))))
          (date-elements->string
            (match date-elements
              ((list y)     (list #f #f y))
              ((list m y)   (list #f m y))
              ((list m d y) (list d m y))
              (_ date-elements))))))
  (define clinical-trials (jsexpr-object-ref-recursive trial-data
                                                       '(StudyFieldsResponse StudyFields)
                                                       '()))
  (let loop ((ctes clinical-trials)
             (expanded-evidence expanded-evidence))
    (if (null? ctes)
        expanded-evidence
        (let*-values (((e) (car ctes))
                      ((nctid title start-date end-date summary facility country)
                       (apply values (map (lambda (field)
                                            (let ((v (jsexpr-object-ref e (string->symbol field) '())))
                                              (if (null? v) 'null (car v))))
                                          nct-fields))))
          (loop (cdr ctes)
                (jsexpr-object-set expanded-evidence
                                  (string->symbol nctid)
                                  (make-jsexpr-object
                                    `((type    . "trial")
                                      (url     . ,(id->link nctid))
                                      (title   . ,title)
                                      (dates   . ,(filter (lambda (d) (not (jsexpr-null? d)))
                                                          (map parse-date
                                                               (list start-date end-date))))
                                      (summary . ,summary)
                                      (source  . ,(match `(,facility ,country)
                                                    ((list 'null 'null) 'null)
                                                    ((list f 'null) f)
                                                    ((list 'null c) c)
                                                    (_ (string-append facility ", " country))))))))))))
(define (expand-evidence answers expanders)
  (define id-patterns (map id-pattern expanders))
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
  (define expanded-evidence (foldl (lambda (expander ids evidence)
                                     (let ((data ((ids->data expander) ids)))
                                       ((data-parser expander) data evidence)))
                                 (jsexpr-object)
                                 expanders
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
    (define (date>=? a b)
      (let ((res (foldl (lambda (ae be cmp)
                          (cond ((not (equal? cmp '=)) cmp)
                                ((string>? ae be) '>)
                                ((string<? ae be) '<)
                                (else '=)))
                        '=
                        (reverse (string-split a "/"))
                        (reverse (string-split b "/")))))
        (not (equal? res '<))))

    (define publication-dates (filter (lambda (d) (and (not (jsexpr-null? d)) d))
                                      (map (lambda (e)
                                             (let ((dates (jsexpr-object-ref e 'dates)))
                                               (and (not (null? dates)) (last dates))))
                                      (jsexpr-object-ref-recursive answer '(edge evidence)))))
    (jsexpr-object-set-recursive answer
                                 '(edge last_publication_date)
                                 (if (null? publication-dates)
                                     'null
                                     (car (sort publication-dates date>=?)))))
