#lang racket/base

(provide
  server-config 
  config-server-mode
  config-document-root
  server-config-exception)

(struct config (server-mode document-root))
(struct server-config-exception exn:fail:user ())

(define server-config (config
  'demo ; Server mode ('dev or 'demo)
  (path->string (current-directory)) ; Document root for the server
))
