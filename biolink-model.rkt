#lang racket

(provide
 (struct-out biolink-data)
 BIOLINK-PREDICATES
 sanitize-predicate
 biolinkify-predicate
 biolink-predicate?
 invert-biolink-predicate
 get-biolink-predicate-data
 biolink-predicate-more-specific-than?
 sort-biolink-predicates
 )

(require
  json
  "common.rkt")


(define j (with-input-from-file "assets/biolink-model.json" (lambda () (read-json)))) ; < 1 second to read json file directly converted via yq
(define slots (jsexpr-object-ref j 'slots))

(struct biolink-data
  (parent        ; value of is-a
   canonical?    ; value/presence of annotations.canonical_predicate
   symmetric?    ; value of property `symmetric`
   deprecated?   ; presence/value of `deprecated`
   inverse?      ; actually has an inverse property
   inverse-pred  ; the inverse of the given predicate
   rank          ; distance from related-to, with related-to itself having rank 0
   raw-data      ; the full record as read in via the json file
   ))

;; For the following get-<fieldname> set of functions, pred is a key in the 'slots hash and record is the value for that key
(define (get-parent pred record)
  (if (eq? pred '|related to|)
      #f
      (jsexpr-object-ref record 'is_a #f)))

(define (get-canonical? pred record)
  (jsexpr-object-ref-recursive record '(annotations canonical_predicate) #f))

(define (get-symmetric? pred record)
  (jsexpr-object-ref record 'symmetric #f))

(define (get-deprecated? pred record)
  (jsexpr-object-ref record 'deprecated #f))

(define (get-inverse? pred record)
  (hash-has-key? record 'inverse))

(define (get-inverse-pred pred record)
  (if (jsexpr-object-ref record 'symmetric #f)
      (symbol->string pred)
      (jsexpr-object-ref record 'inverse #f)))

(define (get-raw-data pred record) record)

(define (distance-from-related-to slots-hash predicate)
  (define (aux slots-hash predicate level)
    (let ((cur (jsexpr-object-ref slots-hash predicate #f)))
      (cond
       ((eq? predicate '|related to|) level)
       ((and cur (jsexpr-object-has-key? cur 'is_a))
        (aux slots (string->symbol (jsexpr-object-ref cur 'is_a)) (add1 level)))
       (else +nan.0))))
  (aux slots-hash predicate 0))

(define (create-bl-predicates slots-hash)
  (define initial-preds
    (apply hash
           (flatten
            (hash-map slots-hash
                      (lambda (k v)
                        (let ((rank (distance-from-related-to slots-hash k)))
                          (if (nan? rank)
                            '() ; this is not a predicate
                            (list (symbol->string k)
                                  (biolink-data
                                   (get-parent k v)
                                   (get-canonical? k v)
                                   (get-symmetric? k v)
                                   (get-deprecated? k v)
                                   (get-inverse? k v)
                                   (get-inverse-pred k v)
                                   rank
                                   (get-raw-data k v))))))))))
  ; With that done, loop over the data structure setting all inverses for predicates in the
  ; reverse direction wrt how they are specified in the raw data, to make the inverse-pred
  ; property fully bi-directional
  (let loop ((keys (hash-keys initial-preds))
             (retval initial-preds))
    (if (null? keys)
        retval
        (let* ((cur-key (car keys))
               (cur-record (hash-ref retval cur-key)))
          (if (biolink-data-inverse? cur-record)
              (let* ((target-record-key (biolink-data-inverse-pred cur-record))
                     (target-record (hash-ref retval target-record-key)))
                (loop (cdr keys) (hash-set retval target-record-key (struct-copy biolink-data target-record (inverse-pred cur-key)))))
              (loop (cdr keys) retval))))))

(define BIOLINK-PREDICATES (create-bl-predicates slots))

(define (sanitize-predicate s)
  (string-replace (string-replace s "_" " ") "biolink:" ""))

(define (biolinkify-predicate s)
  (let ((s (string-replace s " " "_")))
    (if (string-prefix? s "biolink:")
        s
        (string-append "biolink:" s))))

(define (biolink-predicate? s)
  (hash-has-key? BIOLINK-PREDICATES (sanitize-predicate s)))

(define (invert-biolink-predicate p (biolinkify #f))
  (let* ((p (sanitize-predicate p))
         (data (hash-ref BIOLINK-PREDICATES p #f)))
    (if data
        (if biolinkify
            (biolinkify-predicate (biolink-data-inverse-pred data))
            (biolink-data-inverse-pred data))
        (raise-argument-error 'invert-biolink-predicate "<a valid biolink predicate>" p))))

(define (get-biolink-predicate-data p)
  (hash-ref BIOLINK-PREDICATES (sanitize-predicate p) #f))

(define (biolink-predicate-more-specific-than? p1 p2)
  (let* ((p1 (sanitize-predicate p1))
         (p2 (sanitize-predicate p2))
         (p1-data (get-biolink-predicate-data p1))
         (p2-data (get-biolink-predicate-data p2)))
    (cond
     ((not p1-data) (raise-argument-error 'biolink-predicate-more-specific-than? "<a valid biolink predicate>" p1))
     ((not p2-data) (raise-argument-error 'biolink-predicate-more-specific-than? "<a valid biolink predicate>" p2))
     (else (> (biolink-data-rank p1-data) (biolink-data-rank p2-data))))))

;; "More specific" predicates sort before more general ones. Greater specificity
;; is defined as distance from "related to". This is not a very scientific
;; measure, but will do for MVP.
(define (sort-biolink-predicates pred-list)
  (sort pred-list biolink-predicate-more-specific-than?))


(define (tests)
  (and
   (let ((b (get-biolink-predicate-data "biolink:is_frameshift_variant_of")))
     (not (biolink-data-symmetric? b)))
   (string=? (invert-biolink-predicate "biolink:is_frameshift_variant_of")  "has frameshift variant")
   (string=? (invert-biolink-predicate "biolink:is_frameshift_variant_of" #t)  "biolink:has_frameshift_variant")
   (not (biolink-predicate? "biolink:mysteries_of_the_maya"))
   (biolink-predicate? "treats")
   (not (biolink-predicate? "is treated by"))
   (biolink-predicate? "treated by")
   (biolink-predicate? "biolink:treated_by")
   (biolink-data-symmetric? (get-biolink-predicate-data "exact match"))
   (string=? (invert-biolink-predicate "exact match") "exact match")
   (= (distance-from-related-to slots '|related to|) 0)
   (= (distance-from-related-to slots '|treated by|) 4)
   (= (distance-from-related-to slots '|is sequence variant of|) 2)
   (= (distance-from-related-to slots '|related to at concept level|) 1)
   (nan? (distance-from-related-to slots '|hello hello|))
   (let ((sortme '("related to" "treated by" "related to at concept level" "is sequence variant of"))
         (sorted '("treated by" "is sequence variant of" "related to at concept level" "related to")))
     (and (equal? sorted (sort-biolink-predicates sortme))
          (equal? (map biolinkify-predicate sorted) (sort-biolink-predicates
                                                     (map biolinkify-predicate sortme)))))
  ))
