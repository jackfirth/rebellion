#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [splicing-between (-> (sequence/c any/c) transducer?)]))


(require racket/match
         racket/sequence
         rebellion/base/option
         rebellion/base/variant
         rebellion/private/static-name
         rebellion/streaming/transducer/base
         rebellion/type/record)


;@----------------------------------------------------------------------------------------------------


(define-record-type element-emit-state (previous-element buffered-element))
(define-record-type in-between-emit-state (splice-element splice-iterator buffered-element))


(define (splicing-between seq)
  (make-transducer

   #:starter (λ () (variant #:consume absent))

   #:consumer
   (λ (previous-element-opt next)
     (match previous-element-opt
       [(present e)
        (variant #:emit (element-emit-state #:previous-element e #:buffered-element next))]
       [(== absent) (variant #:consume (present next))]))

   #:emitter
   (λ (state)
     (match state
       [(element-emit-state #:previous-element prev #:buffered-element buffered)
        (define-values (head iterator) (sequence-generate* seq))
        (define next-state
          (match head
            [(list e)
             (define s
               (in-between-emit-state
                #:splice-element e #:splice-iterator iterator #:buffered-element buffered))
             (variant #:emit s)]
            [#false (variant #:consume (present buffered))]))
        (emission next-state prev)]
       [(in-between-emit-state
         #:splice-element head #:splice-iterator iterator #:buffered-element buffered)
        (define-values (next-head next-iterator) (iterator))
        (define next-state
          (match next-head
            [(list e)
             (define s
               (in-between-emit-state
                #:splice-element e #:splice-iterator next-iterator #:buffered-element buffered))
             (variant #:emit s)]
            [#false (variant #:consume (present buffered))]))
        (emission next-state head)]))

   #:half-closer
   (λ (previous-element-opt)
     (match previous-element-opt
       [(present e) (variant #:half-closed-emit e)]
       [(== absent) (variant #:finish #false)]))

   #:half-closed-emitter
   (λ (last-element)
     (half-closed-emission (variant #:finish #false) last-element))

   #:finisher void

   #:name (name splicing-between)))
