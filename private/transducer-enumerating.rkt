#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [enumerated? predicate/c]
  [enumerated (-> #:element any/c #:position natural? enumerated?)]
  [enumerated-element (-> enumerated? any/c)]
  [enumerated-position (-> enumerated? natural?)]
  [enumerating transducer?]))

(require racket/math
         rebellion/base/impossible-function
         rebellion/base/variant
         rebellion/streaming/transducer/base
         rebellion/type/record)

;@------------------------------------------------------------------------------

(define-record-type enumerated (element position))

(define enumerating
  (make-transducer
   #:starter (λ () (variant #:consume 0))
   #:consumer
   (λ (position element)
     (variant #:emit (enumerated #:element element #:position position)))
   #:emitter
   (λ (enum)
     (emission (variant #:consume (add1 (enumerated-position enum))) enum))
   #:half-closer (λ (_) (variant #:finish #f))
   #:half-closed-emitter impossible
   #:finisher void
   #:name 'enumerating))
