#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [reducer? predicate/c]
  [reducer/c (-> contract? contract? contract?)]
  [make-reducer
   (->*
    (#:starter (-> reduction-state/c)
     #:consumer (-> any/c any/c reduction-state/c)
     #:finisher (-> any/c any/c)
     #:early-finisher (-> any/c any/c))
    (#:name (or/c interned-symbol? #false))
    reducer?)]
  [reducer-starter (-> reducer? (-> reduction-state/c))]
  [reducer-consumer (-> reducer? (-> any/c any/c reduction-state/c))]
  [reducer-finisher (-> reducer? (-> any/c any/c))]
  [reducer-early-finisher (-> reducer? (-> any/c any/c))]
  [reducer-impersonate
   (->* (reducer?)
        (#:domain-guard (or/c (-> any/c any/c) #false)
         #:range-guard (or/c (-> any/c any/c) #false)
         #:properties impersonator-property-hash/c
         #:chaperone? boolean?)
        reducer?)]))


(require racket/bool
         racket/contract/combinator
         rebellion/base/symbol
         rebellion/base/variant
         rebellion/private/contract-projection
         rebellion/private/impersonation
         rebellion/private/static-name
         rebellion/type/object)


;@----------------------------------------------------------------------------------------------------


(define reduction-state/c (variant/c #:consume any/c #:early-finish any/c))


(define-object-type reducer (starter consumer finisher early-finisher)
  #:constructor-name constructor:reducer)


(define (make-reducer #:starter starter*
                      #:consumer consumer*
                      #:finisher finisher*
                      #:early-finisher early-finisher*
                      #:name [name #false])
  (define starter
    (if (zero? (procedure-arity starter*)) starter* (procedure-reduce-arity starter* 0)))
  (define consumer
    (if (equal? (procedure-arity consumer*) 2) consumer* (procedure-reduce-arity consumer* 2)))
  (define finisher
    (if (equal? (procedure-arity finisher*) 1) finisher* (procedure-reduce-arity finisher* 1)))
  (define early-finisher
    (if (equal? (procedure-arity early-finisher*) 1)
        early-finisher*
        (procedure-reduce-arity early-finisher* 1)))
  (constructor:reducer
   #:starter starter
   #:consumer consumer
   #:finisher finisher
   #:early-finisher early-finisher
   #:name name))


;@------------------------------------------------------------------------------
;; Contracts

(define ((reducer-consumer-guard domain-guard) state element)
  (values state (domain-guard element)))

(define (reducer-impersonate
         reducer
         #:domain-guard [domain-guard #false]
         #:range-guard [range-guard #false]
         #:properties [properties (hash)]
         #:chaperone?
         [chaperone? (and (false? domain-guard) (false? range-guard))])
  (define consumer (reducer-consumer reducer))
  (define finisher (reducer-finisher reducer))
  (define early-finisher (reducer-early-finisher reducer))
  (define domain-chaperone? (or chaperone? (false? domain-guard)))
  (define range-chaperone? (or chaperone? (false? range-guard)))

  (define impersonated-consumer
    (function-impersonate
     consumer
     #:arguments-guard (and domain-guard (reducer-consumer-guard domain-guard))
     #:chaperone? domain-chaperone?))

  (define impersonated-finisher
    (function-impersonate finisher
                          #:results-guard range-guard
                          #:chaperone? range-chaperone?))
  
  (define impersonated-early-finisher
    (function-impersonate early-finisher
                          #:results-guard range-guard
                          #:chaperone? range-chaperone?))
  
  (define impersonated-without-props
    (make-reducer #:starter (reducer-starter reducer)
                  #:consumer impersonated-consumer
                  #:finisher impersonated-finisher
                  #:early-finisher impersonated-early-finisher
                  #:name (object-name reducer)))
  
  (object-impersonate impersonated-without-props descriptor:reducer
                      #:properties properties))


(define/name (reducer/c domain-contract* range-contract*)
  (define domain-contract (coerce-contract enclosing-function-name domain-contract*))
  (define range-contract (coerce-contract enclosing-function-name range-contract*))
  (define contract-name
    (build-compound-type-name enclosing-function-name domain-contract range-contract))
  (define domain-projection (contract-late-neg-projection domain-contract))
  (define range-projection (contract-late-neg-projection range-contract))
  (define chaperone? (and (chaperone-contract? domain-contract) (chaperone-contract? range-contract)))
  (define (projection blame)
    (define domain-blame (blame-add-context blame "an element reduced by" #:swap? #true))
    (define range-blame (blame-add-context blame "the reduction result of"))
    (define late-neg-domain-guard (domain-projection domain-blame))
    (define late-neg-range-guard (range-projection range-blame))
    (λ (v missing-party)
      (assert-satisfies v reducer? blame #:missing-party missing-party)
      (define props
        (hash impersonator-prop:contracted the-contract
              impersonator-prop:blame (cons blame missing-party)))
      (define (domain-guard v) (late-neg-domain-guard v missing-party))
      (define (range-guard v) (late-neg-range-guard v missing-party))
      (reducer-impersonate v
                           #:domain-guard domain-guard
                           #:range-guard range-guard
                           #:chaperone? chaperone?
                           #:properties props)))
  (define the-contract
    ((if chaperone? make-chaperone-contract make-contract)
     #:name contract-name
     #:first-order reducer?
     #:late-neg-projection projection))
  the-contract)
