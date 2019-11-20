#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [into-transduced
   (-> transducer? #:into reducer? transducer? ... reducer?)]))

(require racket/list
         rebellion/base/variant
         rebellion/private/strict-cond
         rebellion/streaming/reducer
         rebellion/streaming/transducer/base
         rebellion/streaming/transducer/composition
         rebellion/type/record
         rebellion/type/wrapper)

;@------------------------------------------------------------------------------

(define (into-transduced first-transducer #:into reducer
                         . remaining-transducers)
  (define piped (apply transducer-pipe first-transducer remaining-transducers))
  (into-transduced-without-composition piped reducer))

(define (into-transduced-without-composition transducer reducer)
  (make-reducer 
   #:starter (λ () (start-transduced transducer reducer))
   #:consumer consume-transduced
   #:finisher finish-transduced
   #:early-finisher early-finish-transduced
   #:name 'transduced))

(define-record-type transduced-state
  (upstream-transducer upstream-state downstream-reducer downstream-state))

(define-wrapper-type forcibly-closed-early-result)
(define-wrapper-type reducer-closed-early-result)

(define (early-finish-transduced-state? v)
  (and (transduced-state? v)
       (variant-tagged-as? (transduced-state-downstream-state v)
                           '#:early-finish)))

(define (consume-transduced-state? v)
  (and (transduced-state? v)
       (variant-tagged-as? (transduced-state-upstream-state v) '#:consume)
       (variant-tagged-as? (transduced-state-downstream-state v) '#:consume)))

(define (internal-consume-transduced-state? v)
  (and (transduced-state? v)
       (variant-tagged-as? (transduced-state-upstream-state v) '#:emit)
       (variant-tagged-as? (transduced-state-downstream-state v) '#:consume)))

(define (internal-half-closed-consume-transduced-state? v)
  (and (transduced-state? v)
       (variant-tagged-as? (transduced-state-upstream-state v)
                           '#:half-closed-emit)
       (variant-tagged-as? (transduced-state-downstream-state v) '#:consume)))

(define (internal-finish-transduced-state? v)
  (and (transduced-state? v)
       (variant-tagged-as? (transduced-state-upstream-state v) '#:finish)
       (variant-tagged-as? (transduced-state-downstream-state v) '#:consume)))

(define (resolve-internal-transduced-state-transitions state)
  (define upstream (transduced-state-upstream-transducer state))
  (define upstream-state (transduced-state-upstream-state state))
  (define downstream (transduced-state-downstream-reducer state))
  (define downstream-state (transduced-state-downstream-state state))
  (strict-cond
   [(internal-consume-transduced-state? state)
    (define upstream-emitter (transducer-emitter upstream))
    (define downstream-consumer (reducer-consumer downstream))
    (define em (upstream-emitter (variant-value upstream-state)))
    (define next-upstream-state (emission-state em))
    (define next-downstream-state
      (downstream-consumer (variant-value downstream-state)
                           (emission-value em)))
    (resolve-internal-transduced-state-transitions
     (transduced-state #:upstream-transducer upstream
                       #:upstream-state next-upstream-state
                       #:downstream-reducer downstream
                       #:downstream-state next-downstream-state))]
   [(internal-half-closed-consume-transduced-state? state)
    (define upstream-emitter (transducer-half-closed-emitter upstream))
    (define downstream-consumer (reducer-consumer downstream))
    (define em (upstream-emitter (variant-value upstream-state)))
    (define next-upstream-state (half-closed-emission-state em))
    (define next-downstream-state
      (downstream-consumer (variant-value downstream-state)
                           (half-closed-emission-value em)))
    (resolve-internal-transduced-state-transitions
     (transduced-state #:upstream-transducer upstream
                       #:upstream-state next-upstream-state
                       #:downstream-reducer downstream
                       #:downstream-state next-downstream-state))]
   [else state]))

(define (tag-transduced-state unresolved-state)
  (define state
    (resolve-internal-transduced-state-transitions unresolved-state))
  (define upstream (transduced-state-upstream-transducer state))
  (define upstream-state (transduced-state-upstream-state state))
  (define downstream (transduced-state-downstream-reducer state))
  (define downstream-state (transduced-state-downstream-state state))
  (strict-cond
   [(consume-transduced-state? state) (variant #:consume state)]
   [(internal-finish-transduced-state? state)
    (define upstream-finisher (transducer-finisher upstream))
    (upstream-finisher (variant-value upstream-state))
    (define downstream-finisher (reducer-finisher downstream))
    (define result (downstream-finisher (variant-value downstream-state)))
    (variant #:early-finish result)]
   [(early-finish-transduced-state? state)
    (define downstream-finisher (reducer-early-finisher downstream))
    (define result (downstream-finisher (variant-value downstream-state)))
    (variant #:early-finish result)]))

(define (start-transduced upstream downstream)
  (tag-transduced-state
   (transduced-state
    #:upstream-transducer upstream
    #:upstream-state ((transducer-starter upstream))
    #:downstream-reducer downstream
    #:downstream-state ((reducer-starter downstream)))))

(define (consume-transduced state element)
  (define upstream (transduced-state-upstream-transducer state))
  (define upstream-state (transduced-state-upstream-state state))
  (define downstream (transduced-state-downstream-reducer state))
  (define downstream-state (transduced-state-downstream-state state))
  (define upstream-consumer (transducer-consumer upstream))
  (define next-upstream-state
    (upstream-consumer (variant-value upstream-state) element))
  (tag-transduced-state
   (transduced-state
    #:upstream-transducer upstream
    #:upstream-state next-upstream-state
    #:downstream-reducer downstream
    #:downstream-state downstream-state)))

(define (finish-transduced state)
  #f)

(define (early-finish-transduced result) result)
