#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [make-tuple-constructor-style-custom-write
   (-> tuple-descriptor? custom-write-function/c)]
  [make-tuple-named-object-custom-write
   (->* (tuple-descriptor?)
        (#:name-field (or/c natural? #f))
        custom-write-function/c)]))

(require racket/math
         racket/struct
         rebellion/custom-write
         rebellion/tuple-type)

;@------------------------------------------------------------------------------

(define (make-tuple-constructor-style-custom-write descriptor)
  (define type (tuple-descriptor-type descriptor))
  (define type-name (tuple-type-name type))
  (define size (tuple-type-size type))
  (define accessor (tuple-descriptor-accessor descriptor))
  (make-constructor-style-printer
   (λ (_) type-name)
   (λ (this) (build-list size (λ (pos) (accessor this pos))))))

(define (make-tuple-named-object-custom-write descriptor
                                              #:name-field [name-field #f])
  (define type-name (tuple-type-name (tuple-descriptor-type descriptor)))
  (define accessor (tuple-descriptor-accessor descriptor))
  (define name-getter
    (if name-field (λ (this) (accessor name-field)) object-name))
  (make-named-object-custom-write type-name #:name-getter name-getter))
