#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [make-constructor-style-struct-write-property
   (-> uninitialized-struct-descriptor? (procedure-arity-includes/c 3))]))

(require racket/struct
         rebellion/struct-descriptor)

;@------------------------------------------------------------------------------

(define (struct-descriptor-fields descriptor)
  (+ (struct-descriptor-mutable-fields descriptor)
     (struct-descriptor-immutable-fields descriptor)
     (struct-descriptor-auto-fields descriptor)))

(define (make-constructor-style-struct-write-property descriptor)
  (define name (struct-descriptor-name descriptor))
  (define accessor (struct-descriptor-accessor descriptor))
  (define fields (struct-descriptor-fields descriptor))
  (make-constructor-style-printer
   (λ (_) name)
   (λ (this) (build-list fields (λ (field) (accessor this field))))))
