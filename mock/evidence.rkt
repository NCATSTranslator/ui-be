#lang racket/base

(provide
  make-pmid-expander
  make-nct-expander)

(require
  racket/string
  racket/file
  json
  xml
  xml/path
  "../config.rkt"
  "../common.rkt"
  (prefix-in evi: "../evidence.rkt"))

(define (make-pmid-expander)
  (evi:make-pmid-expander mock-pubmed-fetch))

(define (make-nct-expander)
  (evi:make-nct-expander mock-nct-fetch))

(define (make-id-fetch id-type reader)
  (define document-root (config-document-root SERVER-CONFIG))
  (define evidence-path (string-append document-root
                                       "test/evidence/"
                                       (symbol->string id-type)))
  (lambda (ids)
    (call-with-input-file
      (string-add-prefix evidence-path "/master.json")
      (lambda (in)
        (define id-mocks (read-json in))
        (filter (lambda (resp) resp)
                (map (lambda (id)
                       (define mock-path (jsexpr-object-ref id-mocks (string->symbol id)))
                       (if mock-path
                           (call-with-input-file
                             (string-append document-root mock-path)
                             (lambda (in) (reader in)))
                           #f))
                    ids))))))

(define (mock-pubmed-fetch ids)
  `(PubmedArticleSet ()
    ,@(map (lambda (article-set)
            (define as-xexpr (xml->xexpr (document-element article-set)))
            (tag->xexpr-subtree as-xexpr 'PubmedArticle))
          ((make-id-fetch 'pmid read-xml) ids))))
(define (mock-nct-fetch ids)
  (make-jsexpr-object
    `((StudyFieldsResponse .
       ,(make-jsexpr-object
          `((StudyFields .
             ,((make-id-fetch 'nct read-json) ids))))))))
