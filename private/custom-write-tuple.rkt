#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [make-tuple-constructor-style-custom-write
   (-> tuple-descriptor? custom-write-function/c)]))

(require racket/struct
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
