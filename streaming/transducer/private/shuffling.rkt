#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [shuffling (-> transducer?)]))


(require racket/list
         racket/match
         rebellion/base/impossible-function
         rebellion/base/variant
         rebellion/private/static-name
         rebellion/streaming/transducer/base)


;@----------------------------------------------------------------------------------------------------


(define (shuffling)
  standard-shuffling-transducer)


(define standard-shuffling-transducer
  (make-transducer
   #:starter (λ () (variant #:consume '()))
   #:consumer (λ (previous v) (variant #:consume (cons v previous)))
   #:emitter impossible
   #:half-closer
   (λ (seen)
     (if (empty? seen) (variant #:finish #false) (variant #:half-closed-emit (shuffle seen))))
   #:half-closed-emitter
   (λ (remaining)
     (match remaining
       [(cons v '()) (half-closed-emission (variant #:finish #false) v)]
       [(cons v tail) (half-closed-emission (variant #:half-closed-emit tail) v)]))
   #:finisher void
   #:name (name shuffling)))
