#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [make-struct-constructor-style-custom-write
   (-> struct-descriptor? custom-write-function/c)]))

(require racket/struct
         rebellion/custom-write
         rebellion/type/struct)

;@------------------------------------------------------------------------------

(define (make-struct-constructor-style-custom-write descriptor)
  (define type-name (struct-descriptor-name descriptor))
  (define size
    (+ (struct-descriptor-mutable-fields descriptor)
       (struct-descriptor-immutable-fields descriptor)
       (struct-descriptor-auto-fields descriptor)))
  (define accessor (struct-descriptor-accessor descriptor))
  (make-constructor-style-printer
   (λ (this) type-name)
   (λ (this) (build-list size (λ (pos) (accessor this pos))))))
