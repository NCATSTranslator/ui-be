#lang racket/base

(require
  racket/function
  yaml
  "common.rkt"
)

(provide
  server-config 
  server-config-exception
  ars-config
  curie-search-config 
  (struct-out config)
)

(struct config
  (document-root
   ars-endpoint
   curie-search-endpoint
   eutils-endpoint
   primary-predicates
   id->url 
   mock-ars?
   mock-query?)
  #:prefab)
(struct server-config-exception exn:fail:user ())

(define (make-server-config config-file)
  (define config-data (file->yaml config-file))
  (define (get k) (yaml-ref config-data k))
  (define (default-for v default)
    (if v v default))

  (define (id-url-mappings->proc id-url-mappings)
    (lambda (id)
      (let loop ((res #f)
                 (mappings id-url-mappings))
        (if (null? mappings)
            "Unknown supporting document type"
            (let* ((m (car mappings))
                   (id-pattern (yaml-ref m 'id-pattern))
                   (url-format (yaml-ref m 'url-format))
                   (strip-tag? (yaml-ref m 'strip-tag?)))
              (if (regexp-match? id-pattern id)
                  (format url-format ((if strip-tag? strip-id-tag identity) id))
                  (loop res (cdr mappings))))))))

  (config
    (default-for (get 'document-root) (string-append (path->string (current-directory)) "/"))
    (get 'ars-endpoint) 
    (get 'curie-search-endpoint) 
    (get 'eutils-endpoint) 
    (make-biolink-tags (get 'primary-predicates))
    (id-url-mappings->proc (get 'id-url-mappings))
    (get 'mock-ars?) 
    (get 'mock-query?)))

(define server-config (make-server-config "configurations/production.yaml"))
(define (get-config accessor key)
  (let ((config (accessor server-config)))
    (if config (yaml-ref config key) #f)))
(define (ars-config k) (get-config config-ars-endpoint k))
(define (curie-search-config k) (get-config config-curie-search-endpoint k))

; TODO
; Set configuration file from command line

; evidence.rkt
; specify eutils service