#lang racket/base
(require yaml)
(require json)
(require memo)


;(define y (file->yaml "b.yaml"))     ; 15 fucking seconds!
; (define slots (hash-ref y "slots"))

(define j (with-input-from-file "biolink-model.json" (lambda () (read-json))))
(define slots (hash-ref j 'slots))

(define (my-hash-map-filter hash map-proc filter-proc)
  (filter filter-proc (hash-map hash map-proc)))
; E.g. usage: (my-hash-map-filter slots (lambda (k v) (cons k (hash-has-key? v "is_a"))) (lambda (x) (not (cdr x))))

(define/memoize (is-child-of-related-to slots-hash predicate)
  (let ((cur (hash-ref slots-hash predicate #f)))        
    (cond
      ((not cur) #f)
      ((eq? predicate 'related\ to) #t) ; put this first
      ((not (hash-has-key? cur 'is_a)) #f)
      (else (is-child-of-related-to slots-hash (string->symbol (hash-ref cur 'is_a)))))))

(define slot-graph (make-hash))
(define inverses (make-hash))

(define (build-slot-maps hash slot-graph inverses)
  (hash-for-each
   hash
   (lambda (k v)
     (cond
       ((hash-has-key? v "is_a") (begin
                                   (hash-set! slot-graph k (hash-ref v "is_a"))
                                   (when (hash-has-key? v "inverse") (hash-set! inverses k (hash-ref v "inverse")))))
       ((hash-has-key? v "inverse") (hash-set! inverses k (hash-ref v "inverse")))))))


                   