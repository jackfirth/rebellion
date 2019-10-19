#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [deduplicating transducer?]))

(require racket/contract/region
         racket/set
         rebellion/base/variant
         rebellion/private/impossible
         rebellion/streaming/transducer/base
         rebellion/type/tuple)

;@------------------------------------------------------------------------------

(define-tuple-type emit-state (previously-encountered novelty))

(define deduplicating
  (make-transducer
   #:starter (λ () (variant #:consume (set)))
   #:consumer
   (λ (encountered v)
     (if (set-member? encountered v)
         (variant #:consume encountered)
         (variant #:emit (emit-state encountered v))))
   #:emitter
   (λ (state)
     (define encountered (emit-state-previously-encountered state))
     (define novelty (emit-state-novelty state))
     (emission (variant #:consume (set-add encountered novelty)) novelty))
   #:half-closer (λ (_) (variant #:finish #f))
   #:half-closed-emitter impossible
   #:finisher void
   #:name 'deduplicating))
