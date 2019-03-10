#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [make-tuple-equal+hash
   (-> tuple-descriptor?
       (list/c procedure? procedure? procedure?))]))

(require rebellion/equal+hash
         rebellion/tuple-type)

;@------------------------------------------------------------------------------

(define (make-tuple-equal+hash descriptor)
  (define accessor (tuple-descriptor-accessor descriptor))
  (define size (tuple-type-size (tuple-descriptor-type descriptor)))
  (make-accessor-based-equal+hash accessor size))
