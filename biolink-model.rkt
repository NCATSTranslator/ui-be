#lang racket/base
(require yaml)

(define y (file->yaml "b.yaml"))
(define slots (hash-ref y "slots"))

(define (my-hash-map-filter hash map-proc filter-proc)
  (filter filter-proc (hash-map hash map-proc)))
; E.g. usage: (my-hash-map-filter slots (lambda (k v) (cons k (hash-has-key? v "is_a"))) (lambda (x) (not (cdr x))))

(define slot-graph (make-hash))
(define inverses (make-hash))

(define (traverse-hash hash slot-graph inverses)
  (hash-for-each
   hash
   (lambda (k v)
     (cond
       ((hash-has-key? v "is_a") (begin
                                   (hash-set! slot-graph k (hash-ref v "is_a"))
                                   (when (hash-has-key? v "inverse") (hash-set! inverses k (hash-ref v "inverse")))))
       ((hash-has-key? v "inverse") (hash-set! inverses k (hash-ref v "inverse")))))))


                   