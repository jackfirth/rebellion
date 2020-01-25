#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [transduction-state/c flat-contract?]
  [half-closed-transduction-state/c flat-contract?]
  [transducer? predicate/c]
  [transducer-starter (-> transducer? any)]
  [transducer-consumer (-> transducer? any)]
  [transducer-emitter (-> transducer? any)]
  [transducer-half-closer (-> transducer? any)]
  [transducer-half-closed-emitter (-> transducer? any)]
  [transducer-finisher (-> transducer? any)]
  [emission (-> transduction-state/c any/c emission?)]
  [emission? predicate/c]
  [emission-state (-> emission? transduction-state/c)]
  [emission-value (-> emission? any/c)]
  [half-closed-emission
   (-> half-closed-transduction-state/c any/c half-closed-emission?)]
  [half-closed-emission? predicate/c]
  [half-closed-emission-state
   (-> half-closed-emission? half-closed-transduction-state/c)]
  [half-closed-emission-value (-> half-closed-emission? any/c)]
  [make-transducer
   (->* (#:starter (-> transduction-state/c)
         #:consumer (-> any/c any/c transduction-state/c)
         #:emitter (-> any/c emission?)
         #:half-closer (-> any/c half-closed-transduction-state/c)
         #:half-closed-emitter (-> any/c half-closed-emission?)
         #:finisher (-> any/c void?))
        (#:name (or/c interned-symbol? #f))
        transducer?)]
  [transducer-impersonate
   (->* (transducer?)
        (#:domain-guard (or/c (-> any/c any/c) #f)
         #:range-guard (or/c (-> any/c any/c) #f)
         #:properties impersonator-property-hash/c
         #:chaperone? boolean?)
        transducer?)]))

(require racket/bool
         rebellion/base/symbol
         rebellion/base/variant
         rebellion/private/impersonation
         rebellion/type/object
         rebellion/type/tuple)

;@------------------------------------------------------------------------------

(define transduction-state/c variant?)
(define half-closed-transduction-state/c variant?)

(define-tuple-type emission (state value))
(define-tuple-type half-closed-emission (state value))

(define-object-type transducer
  (starter consumer emitter half-closer half-closed-emitter finisher))

(define (transducer-impersonate
         transducer
         #:domain-guard [domain-guard #f]
         #:range-guard [range-guard #f]
         #:properties [properties (hash)]
         #:chaperone?
         [chaperone? (and (false? domain-guard) (false? range-guard))])
  (define domain-chaperone? (or chaperone? (false? domain-guard)))
  (define range-chaperone? (or chaperone? (false? range-guard)))

  (define consumer
    (function-impersonate
     (transducer-consumer transducer)
     #:arguments-guard
     (and domain-guard (transducer-consumer-guard domain-guard))
     #:chaperone? domain-chaperone?))

  (define emitter
    (function-impersonate
     (transducer-emitter transducer)
     #:results-guard (and range-guard (transducer-emitter-guard range-guard))
     #:chaperone? range-chaperone?))

  (define half-closed-emitter
    (function-impersonate
     (transducer-half-closed-emitter transducer)
     #:results-guard
     (and range-guard (transducer-half-closed-emitter-guard range-guard))
     #:chaperone? range-chaperone?))

  (define impersonated-without-props
    (make-transducer #:starter (transducer-starter transducer)
                     #:consumer consumer
                     #:emitter emitter
                     #:half-closer (transducer-half-closer transducer)
                     #:half-closed-emitter half-closed-emitter
                     #:finisher (transducer-finisher transducer)
                     #:name (object-name transducer)))

  (object-impersonate impersonated-without-props descriptor:transducer
                         #:properties properties))

(define ((transducer-consumer-guard domain-guard) state element)
  (values state (domain-guard element)))

(define ((transducer-emitter-guard range-guard) em)
  (emission (emission-state em) (range-guard (emission-value em))))

(define ((transducer-half-closed-emitter-guard range-guard) em)
  (half-closed-emission (half-closed-emission-state em)
                        (range-guard (half-closed-emission-value em))))
