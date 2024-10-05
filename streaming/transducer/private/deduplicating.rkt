#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [deduplicating (->* () (#:key (-> any/c any/c)) transducer?)]
  [deduplicating-consecutive (->* () (#:key (-> any/c any/c)) transducer?)]))

(require guard
         racket/set
         rebellion/base/impossible-function
         rebellion/base/option
         rebellion/base/variant
         rebellion/streaming/transducer/base
         rebellion/type/record)

;@------------------------------------------------------------------------------

(define-record-type emit-state (previously-encountered novel-element))

(define (deduplicating #:key [key-function values])
  (make-transducer
   #:starter (λ () (variant #:consume (set)))
   #:consumer
   (λ (encountered v)
     (guarded-block
       (define k (key-function v))
       (guard (not (set-member? encountered k)) #:else
         (variant #:consume encountered))
       (define state
         (emit-state #:previously-encountered (set-add encountered k)
                     #:novel-element v))
       (variant #:emit state)))
   #:emitter
   (λ (state)
     (define encountered (emit-state-previously-encountered state))
     (define novel (emit-state-novel-element state))
     (emission (variant #:consume encountered) novel))
   #:half-closer (λ (_) (variant #:finish #f))
   #:half-closed-emitter impossible
   #:finisher void
   #:name 'deduplicating))

(define-record-type consecutive-emit-state (previous-key novel-element))

(define (deduplicating-consecutive #:key [key-function values])
  (make-transducer
   #:starter (λ () (variant #:consume absent))
   #:consumer
   (λ (previous v)
     (guarded-block
       (define k (key-function v))
       (guard (present? previous) #:else
         (variant #:emit
                  (consecutive-emit-state #:previous-key (present k)
                                          #:novel-element v)))
       (guard (not (equal? (present-value previous) k)) #:else
         (variant #:consume previous))
       (variant #:emit
                (consecutive-emit-state #:previous-key (present k)
                                        #:novel-element v))))
   #:emitter
   (λ (state)
     (define previous (consecutive-emit-state-previous-key state))
     (define novel (consecutive-emit-state-novel-element state))
     (emission (variant #:consume previous) novel))
   #:half-closer (λ (_) (variant #:finish #f))
   #:half-closed-emitter impossible
   #:finisher void
   #:name 'deduplicating-consecutive))
