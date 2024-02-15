#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [adding-between (-> any/c transducer?)]))


(require racket/match
         rebellion/base/option
         rebellion/base/variant
         rebellion/private/static-name
         rebellion/streaming/transducer/base)


;@----------------------------------------------------------------------------------------------------


(define (adding-between v)
  (make-transducer

   #:starter (λ () (variant #:consume absent))

   #:consumer
   (λ (previous-element-opt next)
     (if (present? previous-element-opt)
         (variant #:emit (list previous-element-opt next))
         (variant #:consume (present next))))

   #:emitter
   (λ (state)
     (match-define (list previous-element-opt next) state)
     (match previous-element-opt
       [(present e) (emission (variant #:emit (list absent next)) e)]
       [(== absent) (emission (variant #:consume (present next)) v)]))

   #:half-closer
   (λ (previous-element-opt)
     (match previous-element-opt
       [(present e) (variant #:half-closed-emit e)]
       [(== absent) (variant #:finish #false)]))

   #:half-closed-emitter
   (λ (last-element)
     (half-closed-emission (variant #:finish #false) last-element))

   #:finisher void
   #:name (name adding-between)))
