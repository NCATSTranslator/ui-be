#lang racket/base

(provide qgraph->trapi-query
         disease->creative-query)

(require "../common.rkt")

(define (qgraph->trapi-query qgraph)
  qgraph
)

(define (disease->creative-query disease)
  (jsexpr-object-ref disease 'disease)
)
