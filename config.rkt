#lang racket/base

(require
  racket/function
  racket/vector
  racket/pretty
  yaml
  "common.rkt"
)

(provide
  SERVER-CONFIG 
  server-config-exception
  ars-config
  curie-search-config 
  (struct-out config)
)

(struct config
  (document-root
   port
   response-timeout
   ars-endpoint
   curie-search-endpoint
   primary-predicates
   mock-ars?
   mock-query?
   yaml)
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
    (get 'port)
    (get 'response-timeout)
    (get 'ars-endpoint) 
    (get 'curie-search-endpoint) 
    (make-biolink-tags (get 'primary-predicates))
    (get 'mock-ars?) 
    (get 'mock-query?)
    config-data))

(define SERVER-CONFIG
  (let ((cmd-args (current-command-line-arguments)))
    (make-server-config (if (vector-empty? cmd-args)
                            "configurations/full-mock.yaml"
                            (vector-ref cmd-args 0)))))
(define (get-config accessor key)
  (let ((config (accessor SERVER-CONFIG)))
    (if config (yaml-ref config key) #f)))
(define (ars-config k) (get-config config-ars-endpoint k))
(define (curie-search-config k) (get-config config-curie-search-endpoint k))

(pretty-display "\nSERVER CONFIGURATION")
(pretty-display (yaml->string (config-yaml SERVER-CONFIG)))