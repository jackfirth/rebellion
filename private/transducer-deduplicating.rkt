#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [deduplicating (->* () (#:key (-> any/c any/c)) transducer?)]))

(require racket/contract/region
         racket/set
         rebellion/base/equivalence-relation
         rebellion/base/variant
         rebellion/private/impossible
         rebellion/streaming/transducer/base
         rebellion/type/record)

;@------------------------------------------------------------------------------

(define-record-type emit-state (previously-encountered novelty))

(define (deduplicating #:key [key-function values])
  (make-transducer
   #:starter (λ () (variant #:consume (set)))
   #:consumer
   (λ (encountered v)
     (define k (key-function v))
     (cond
       [(set-member? encountered k) (variant #:consume encountered)]
       [else
        (define state
          (emit-state #:previously-encountered (set-add encountered k)
                      #:novelty v))
        (variant #:emit state)]))
   #:emitter
   (λ (state)
     (define encountered (emit-state-previously-encountered state))
     (define novelty (emit-state-novelty state))
     (emission (variant #:consume encountered) novelty))
   #:half-closer (λ (_) (variant #:finish #f))
   #:half-closed-emitter impossible
   #:finisher void
   #:name 'deduplicating))
