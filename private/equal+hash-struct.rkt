#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [make-struct-equal+hash
   (-> struct-descriptor?
       (list/c procedure? procedure? procedure?))]))

(require rebellion/equal+hash
         rebellion/struct-descriptor)

;@------------------------------------------------------------------------------

(define (make-struct-equal+hash descriptor)
  (define accessor (struct-descriptor-accessor descriptor))
  (define size
    (+ (struct-descriptor-mutable-fields descriptor)
       (struct-descriptor-immutable-fields descriptor)
       (struct-descriptor-auto-fields descriptor)))
  (make-accessor-based-equal+hash accessor size))
