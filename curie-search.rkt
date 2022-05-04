#lang racket/base

(require
  racket/match
  net/http-client
  json
  "common.rkt"
  "config.rkt")

(provide 
  curie-search
  nr-searcher)

; For now get all matching CURIEs 
(define (parse-curie-search-result jse)
  (map symbol->string (jsexpr-object-keys jse)))

(define (curie-search searcher term offset limit)
  (parse-curie-search-result (searcher term offset limit)))

(define nr-host (curie-search-config 'host))
(define (nr-lookup-uri term offset limit)
  (format "~a~a" (curie-search-config 'uri)
                 (make-url-params `(("string" . ,term)
                                    ("offset" . ,(number->string offset))
                                    ("limit"  . ,(number->string limit))))))
(define (nr-searcher term offset limit)
  (match-define-values (_ _ resp-in)
    (http-sendrecv nr-host
                  (nr-lookup-uri term offset limit)
                  #:ssl? #t
                  #:method #"POST"
                  #:headers `(,header:acc/json)))
  (read-json resp-in))

(module+ test
  (require rackunit)

  (define (generate-test-searcher test-file)
    (lambda (term offset limit)
      (read-json (open-input-file test-file))))

  (define invalid-json
    (generate-test-searcher "test/json/invalid.json"))
  (check-exn
    exn:fail:read?
    (lambda ()
      (curie-search invalid-json "cheese" 0 10)))

  (define valid-json-array
    (generate-test-searcher "test/json/array-simple.json"))
  (check-exn
    exn:fail:contract?
    (lambda ()
      (curie-search valid-json-array "cheese" 0 10)))
  
  (define valid-json-object
    (generate-test-searcher "test/json/object-simple.json"))
  (check-equal?
    (sort (curie-search valid-json-object "cheese" 0 10) string<?)
    '("a" "b" "c"))
)