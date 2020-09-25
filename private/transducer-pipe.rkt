#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [transducer-pipe (-> transducer? ... transducer?)]
  [transducer-compose (-> transducer? ... transducer?)]))

(require racket/bool
         racket/contract/region
         racket/list
         racket/match
         rebellion/base/impossible-function
         rebellion/base/variant
         rebellion/collection/immutable-vector
         rebellion/private/static-name
         rebellion/streaming/transducer/base
         rebellion/type/record)

;@------------------------------------------------------------------------------
;; Wrappers that provide a nicer API over core binary composition

(define (transducer-pipe . transducers-list)
  (if (empty? transducers-list)
      identity-transducer
      (for/fold ([piped (first transducers-list)])
                ([trans (in-list (rest transducers-list))])
        (transducer-binary-pipe piped trans))))

(define (transducer-compose . transducers)
  (apply transducer-pipe (reverse transducers)))

(define empty-consume-state (variant #:consume #f))
(define empty-finish-state (variant #:finish #f))

(define/name identity-transducer
  (make-transducer
   #:starter (λ () empty-consume-state)
   #:consumer (λ (_ v) (variant #:emit v))
   #:emitter (λ (v) (emission empty-consume-state v))
   #:half-closer (λ (_) empty-finish-state)
   #:half-closed-emitter impossible
   #:finisher void
   #:name enclosing-variable-name))

;@------------------------------------------------------------------------------
;; Core binary composition

(define-record-type pipe-state
  (upstream-transducer
   downstream-transducer
   upstream-state
   downstream-state))

(define (emit-pipe-state? v)
  (cond
    [(not (pipe-state? v)) #false]
    [else
     (define upstream (pipe-state-upstream-state v))
     (define downstream (pipe-state-downstream-state v))
     (and (variant-tagged-as? downstream '#:emit)
          (or (not (variant? upstream))
              (and (not (variant-tagged-as? upstream '#:half-closed-emit))
                   (not (variant-tagged-as? upstream '#:finish)))))]))

(define (half-closed-emit-pipe-state? v)
  (cond
    [(not (pipe-state? v)) #false]
    [else
     (define upstream (pipe-state-upstream-state v))
     (define downstream (pipe-state-downstream-state v))
     (or (variant-tagged-as? downstream '#:half-closed-emit)
         (and (variant-tagged-as? downstream '#:emit)
              (variant? upstream)
              (or (variant-tagged-as? upstream '#:half-closed-emit)
                  (variant-tagged-as? upstream '#:finish))))]))

(define (finish-pipe-state? v)
  (and (pipe-state? v)
       (variant-tagged-as? (pipe-state-downstream-state v) '#:finish)))

(define (consume-pipe-state? v)
  (and (pipe-state? v)
       (variant? (pipe-state-upstream-state v))
       (variant-tagged-as? (pipe-state-upstream-state v) '#:consume)
       (variant-tagged-as? (pipe-state-downstream-state v) '#:consume)))

(define (internal-consume-pipe-state? v)
  (and (pipe-state? v)
       (variant? (pipe-state-upstream-state v))
       (variant-tagged-as? (pipe-state-upstream-state v) '#:emit)
       (variant-tagged-as? (pipe-state-downstream-state v) '#:consume)))

(define (internal-half-closed-consume-pipe-state? v)
  (and (pipe-state? v)
       (variant? (pipe-state-upstream-state v))
       (variant-tagged-as? (pipe-state-upstream-state v) '#:half-closed-emit)
       (variant-tagged-as? (pipe-state-downstream-state v) '#:consume)))

(define (internal-finish-pipe-state? v)
  (and (pipe-state? v)
       (variant? (pipe-state-upstream-state v))
       (variant-tagged-as? (pipe-state-upstream-state v) '#:finish)))

(define (internal-half-close-pipe-state? v)
  (and (pipe-state? v)
       (false? (pipe-state-upstream-state v))
       (variant-tagged-as? (pipe-state-downstream-state v) '#:consume)))

(define (resolve-internal-pipe-state-transitions state)
  (define upstream (pipe-state-upstream-transducer state))
  (define upstream-state (pipe-state-upstream-state state))
  (define downstream (pipe-state-downstream-transducer state))
  (define downstream-state (pipe-state-downstream-state state))
  (cond
    [(internal-half-close-pipe-state? state)
     (define downstream-half-closer (transducer-half-closer downstream))
     (define next-downstream-state
       (downstream-half-closer (variant-value downstream-state)))
     (resolve-internal-pipe-state-transitions
      (pipe-state #:upstream-transducer #f
                  #:upstream-state #f
                  #:downstream-transducer downstream
                  #:downstream-state next-downstream-state))]

    [(internal-finish-pipe-state? state)
     (define upstream-finisher (transducer-finisher upstream))
     (upstream-finisher (variant-value upstream-state))
     (resolve-internal-pipe-state-transitions
      (pipe-state #:upstream-transducer #f
                  #:upstream-state #f
                  #:downstream-transducer downstream
                  #:downstream-state downstream-state))]

    [(internal-consume-pipe-state? state)
     (define upstream-emitter (transducer-emitter upstream))
     (define downstream-consumer (transducer-consumer downstream))
     (define em (upstream-emitter (variant-value upstream-state)))
     (define next-upstream-state (emission-state em))
     (define next-downstream-state
       (downstream-consumer (variant-value downstream-state)
                            (emission-value em)))
     (resolve-internal-pipe-state-transitions
      (pipe-state #:upstream-transducer upstream
                  #:upstream-state next-upstream-state
                  #:downstream-transducer downstream
                  #:downstream-state next-downstream-state))]

    [(internal-half-closed-consume-pipe-state? state)
     (define upstream-emitter (transducer-half-closed-emitter upstream))
     (define downstream-consumer (transducer-consumer downstream))
     (define em (upstream-emitter (variant-value upstream-state)))
     (define next-upstream-state (half-closed-emission-state em))
     (define next-downstream-state
       (downstream-consumer (variant-value downstream-state)
                            (half-closed-emission-value em)))
     (resolve-internal-pipe-state-transitions
      (pipe-state #:upstream-transducer upstream
                  #:upstream-state next-upstream-state
                  #:downstream-transducer downstream
                  #:downstream-state next-downstream-state))]

    [else state]))

(define (tag-pipe-state unresolved-state)
  (define state (resolve-internal-pipe-state-transitions unresolved-state))
  (cond
    [(emit-pipe-state? state) (variant #:emit state)]
    [(half-closed-emit-pipe-state? state) (variant #:half-closed-emit state)]
    [(finish-pipe-state? state) (variant #:finish state)]
    [(consume-pipe-state? state) (variant #:consume state)]))

(define (pipe-start upstream downstream)
  (tag-pipe-state
   (pipe-state #:upstream-transducer upstream
               #:upstream-state ((transducer-starter upstream))
               #:downstream-transducer downstream
               #:downstream-state ((transducer-starter downstream)))))

(define (pipe-consume state element)
  (define upstream (pipe-state-upstream-transducer state))
  (define upstream-state (pipe-state-upstream-state state))
  (define downstream (pipe-state-downstream-transducer state))
  (define downstream-state (pipe-state-downstream-state state))
  (define upstream-consumer (transducer-consumer upstream))
  (define next-upstream-state
    (upstream-consumer (variant-value upstream-state) element))
  (tag-pipe-state
   (pipe-state #:upstream-transducer upstream
               #:upstream-state next-upstream-state
               #:downstream-transducer downstream
               #:downstream-state downstream-state)))

(define (pipe-emit state)
  (define upstream (pipe-state-upstream-transducer state))
  (define upstream-state (pipe-state-upstream-state state))
  (define downstream (pipe-state-downstream-transducer state))
  (define downstream-state (pipe-state-downstream-state state))
  (define downstream-emitter (transducer-emitter downstream))
  (define downstream-emission
    (downstream-emitter (variant-value downstream-state)))
  (define next-downstream-state (emission-state downstream-emission))
  (emission (tag-pipe-state
             (pipe-state #:upstream-transducer upstream
                         #:upstream-state upstream-state
                         #:downstream-transducer downstream
                         #:downstream-state next-downstream-state))
            (emission-value downstream-emission)))

(define (pipe-half-close state)
  (define upstream (pipe-state-upstream-transducer state))
  (define upstream-state (pipe-state-upstream-state state))
  (define downstream (pipe-state-downstream-transducer state))
  (define downstream-state (pipe-state-downstream-state state))
  (define upstream-half-closer (transducer-half-closer upstream))
  (define next-upstream-state
    (upstream-half-closer (variant-value upstream-state)))
  (tag-pipe-state
   (pipe-state #:upstream-transducer upstream
               #:upstream-state next-upstream-state
               #:downstream-transducer downstream
               #:downstream-state downstream-state)))

(define (pipe-half-closed-emit state)
  (define upstream (pipe-state-upstream-transducer state))
  (define upstream-state (pipe-state-upstream-state state))
  (define downstream (pipe-state-downstream-transducer state))
  (define downstream-state (pipe-state-downstream-state state))
  (define downstream-emitter (transducer-emitter downstream))
  (define downstream-half-closed-emitter
    (transducer-half-closed-emitter downstream))
  (define (build-emission next-downstream-state emitted-value)
    (half-closed-emission
     (tag-pipe-state
      (pipe-state
       #:upstream-transducer upstream
       #:upstream-state upstream-state
       #:downstream-transducer downstream
       #:downstream-state next-downstream-state))
     emitted-value))
  (match downstream-state
    [(variant #:emit s)
     (define em (downstream-emitter s))
     (build-emission (emission-state em) (emission-value em))]
    [(variant #:half-closed-emit s)
     (define em (downstream-half-closed-emitter s))
     (build-emission
      (half-closed-emission-state em)
      (half-closed-emission-value em))]))

(define (pipe-finish state)
  (define downstream (pipe-state-downstream-transducer state))
  (define downstream-state (pipe-state-downstream-state state))
  (define downstream-finisher (transducer-finisher downstream))
  (downstream-finisher (variant-value downstream-state)))

(define (transducer-binary-pipe upstream downstream)
  (make-transducer
   #:starter (λ () (pipe-start upstream downstream))
   #:consumer pipe-consume
   #:emitter pipe-emit
   #:half-closer pipe-half-close
   #:half-closed-emitter pipe-half-closed-emit
   #:finisher pipe-finish
   #:name 'piped))
