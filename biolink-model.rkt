#lang racket

(provide
 (struct-out biolink-data)
 BIOLINK-PREDICATES
 sanitize-predicate
 biolinkify-predicate
 biolink-predicate?
 invert-biolink-predicate
 get-biolink-predicate-data
 )

(require
  json
  "common.rkt")


(define j (with-input-from-file "assets/biolink-model.json" (lambda () (read-json)))) ; < 1 second to read json file directly converted via yq
(define slots (jsexpr-object-ref j 'slots))

(struct biolink-data
  (parent         ; value of is-a
   is-canonical?   ; value/presence of annotations.canonical_predicate
   is-symmetric?   ; value of property `symmetric`
   is-deprecated?  ; presence/value of `deprecated`
   is-inverse?     ; actually has an inverse property
   inverse-pred   ; the inverse of the given predicate
   raw-data       ; the full record as read in via the json file
   ))

;; For the following get-<fieldname> set of functions, pred is a key in the 'slots hash and record is the value for that key
(define (get-parent pred record)
  (if (eq? pred '|related to|)
      #f
      (jsexpr-object-ref record 'is_a #f)))

(define (get-is-canonical? pred record)
  (jsexpr-object-ref-recursive record '(annotations canonical_predicate) #f))

(define (get-is-symmetric? pred record)
  (jsexpr-object-ref record 'symmetric #f))

(define (get-is-deprecated? pred record)
  (jsexpr-object-ref record 'deprecated #f))

(define (get-is-inverse? pred record)
  (hash-has-key? record 'inverse))

(define (get-inverse-pred pred record)
  (if (jsexpr-object-ref record 'symmetric #f)
      (symbol->string pred)
      (jsexpr-object-ref record 'inverse #f)))

(define (get-raw-data pred record) record)

(define (is-a-related-to slots-hash predicate)
  (let ((cur (jsexpr-object-ref slots-hash predicate #f)))
    (and
     cur
     (or (eq? predicate '|related to|)
         (and (jsexpr-object-has-key? cur 'is_a)
              (is-a-related-to slots-hash (string->symbol (jsexpr-object-ref cur 'is_a))))))))

(define (create-bl-predicates slots-hash)
  (define initial-preds
    (apply hash
           (flatten
            (hash-map slots-hash
                      (lambda (k v)
                        (if (is-a-related-to slots-hash k)
                            (list (symbol->string k)
                                  (biolink-data
                                   (get-parent k v) 
                                   (get-is-canonical? k v)
                                   (get-is-symmetric? k v)
                                   (get-is-deprecated? k v)
                                   (get-is-inverse? k v)
                                   (get-inverse-pred k v)
                                   (get-raw-data k v)))
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
          (if (biolink-data-is-inverse? cur-record)
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
        (raise-argument-error 'invert-biolink-predicate "<a predicate in the BIOLINK-PREDICATES table>" p))))

(define (get-biolink-predicate-data p)
  (hash-ref BIOLINK-PREDICATES (sanitize-predicate p) #f))

;; $ racket
;; Welcome to Racket v8.4 [cs].
;; > (require "biolink-model.rkt")
;; > (define b (get-biolink-predicate-data "biolink:is_frameshift_variant_of"))
;; > (biolink-data-is-symmetric b)
;; #f
;; > (invert-biolink-predicate "biolink:is_frameshift_variant_of")
;; "has frameshift variant"
;; > (invert-biolink-predicate "biolink:is_frameshift_variant_of" #t)
;; "biolink:has_frameshift_variant"
;; > (biolink-predicate? "biolink:mysteries of the maya")
;; #f
;; > (biolink-predicate? "treats")
;; #t
;; > (biolink-predicate? "is treated by")
;; #f
;; > (biolink-predicate? "treated by")
;; #t
;; > (biolink-predicate? "biolink:treated_by")
;; #t
;; > (biolink-data-is-symmetric (get-biolink-predicate-data "exact match"))
;; #t
;; > (invert-biolink-predicate "exact match")
;; "exact match"
