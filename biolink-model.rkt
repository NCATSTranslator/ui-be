#lang racket

(require
  json
  memo
  "common.rkt")

;(require yaml)
;(define y (file->yaml "b.yaml"))     ; 15 seconds! Forget yaml
;(define slots (hash-ref y "slots"))

(define j (with-input-from-file "assets/biolink-model.json" (lambda () (read-json)))) ; < 1 second to read json file directly converted via yq
(define slots (hash-ref j 'slots))

(struct biolink-data
  (parent         ; value of is-a
   is-canonical   ; value/presence of annotations.canonical_predicate
   is-symmetric   ; value of property `symmetric`
   is-deprecated  ; presence/value of `deprecated`
   is-inverse     ; actually has an inverse property
   inverse-pred   ; the inverse of the given predicate
   raw-data       ; the full record as read in via the json file
   ))

;; For the following mk-<fieldname> set of functions, pred is a key in the 'slots hash and record is the value for that key
(define (mk-parent pred record)
  (if (eq? pred 'related\ to)
      #f
      (hash-ref record 'is_a #f)))

(define (mk-is-canonical pred record)
  (jsexpr-object-ref-recursive record '(annotations canonical_predicate) #f))

(define (mk-is-symmetric pred record)
  (hash-ref record 'symmetric #f))

(define (mk-is-deprecated pred record)
  (hash-ref record 'deprecated #f))

(define (mk-is-inverse pred record)
  (hash-has-key? record 'inverse))

(define (mk-inverse-pred pred record)
  (hash-ref record 'inverse #f))

(define (mk-raw-data pred record) record)

(define/memoize (is-a-related-to slots-hash predicate)
  (let ((cur (hash-ref slots-hash predicate #f)))        
    (cond ; mind the order of clauses
      ((not cur) #f)
      ((eq? predicate 'related\ to) #t)
      ((not (hash-has-key? cur 'is_a)) #f)
      (else (is-a-related-to slots-hash (string->symbol (hash-ref cur 'is_a)))))))

(define (create-bl-predicates slots-hash)
  (define initial-preds
    (apply hash
         (flatten
          (hash-map slots-hash
                    (lambda (k v)
                      (if (is-a-related-to slots-hash k)
                          (list (symbol->string k)
                                (biolink-data
                                 (mk-parent k v) 
                                 (mk-is-canonical k v)
                                 (mk-is-symmetric k v)
                                 (mk-is-deprecated k v)
                                 (mk-is-inverse k v)
                                 (mk-inverse-pred k v)
                                 (mk-raw-data k v)))
                          ; else this is not a predicate
                          '()))))))
  ; With that done, loop over the data structure setting all inverses for predicates in the
  ; reverse direction wrt how they are specified in the raw data, to make the inverse-pred
  ; property fully bi-directional
  (let loop ((keys (hash-keys initial-preds))
             (retval initial-preds))
    (if (null? keys)
        retval
        (let* ((cur-key (car keys))
               (cur-record (hash-ref retval cur-key)))
          (if (biolink-data-is-inverse cur-record)
              (let* ((target-record-key (biolink-data-inverse-pred cur-record))
                     (target-record (hash-ref retval target-record-key)))
                (loop (cdr keys) (hash-set retval target-record-key (struct-copy biolink-data target-record (inverse-pred cur-key)))))
              ; else
              (loop (cdr keys) retval))))))

; (define BIOLINK_MODEL (create-initial-preds slots))
(define BIOLINK_MODEL (create-bl-predicates slots))
                         
         