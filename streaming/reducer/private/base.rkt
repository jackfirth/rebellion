#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  #:unprotected-submodule no-contract
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
        reducer?)]
  [branchless-reducer? predicate/c]
  [make-branchless-reducer
   (->* (#:starter (-> any/c)
         #:consumer (-> any/c any/c any/c)
         #:finisher (-> any/c any/c))
        (#:name (or/c interned-symbol? #false))
        reducer?)]
  [branchless-reducer-starter (-> branchless-reducer? (-> any/c))]
  [branchless-reducer-consumer (-> branchless-reducer? (-> any/c any/c any/c))]))


(require racket/bool
         racket/contract/combinator
         rebellion/base/symbol
         rebellion/base/variant
         rebellion/private/contract-projection
         rebellion/private/guarded-block
         rebellion/private/impersonation
         rebellion/private/static-name
         rebellion/type/object)


;@----------------------------------------------------------------------------------------------------


(define reduction-state/c (variant/c #:consume any/c #:early-finish any/c))


(define-object-type generic-reducer (starter consumer finisher early-finisher)
  #:constructor-name constructor:generic-reducer)


(define (make-reducer #:starter starter*
                      #:consumer consumer*
                      #:finisher finisher*
                      #:early-finisher early-finisher*
                      #:name [name #false])
  (define starter
    (if (equal? (procedure-arity starter*) 0) starter* (procedure-reduce-arity starter* 0)))
  (define consumer
    (if (equal? (procedure-arity consumer*) 2) consumer* (procedure-reduce-arity consumer* 2)))
  (define finisher
    (if (equal? (procedure-arity finisher*) 1) finisher* (procedure-reduce-arity finisher* 1)))
  (define early-finisher
    (if (equal? (procedure-arity early-finisher*) 1)
        early-finisher*
        (procedure-reduce-arity early-finisher* 1)))
  (constructor:generic-reducer
   #:starter starter
   #:consumer consumer
   #:finisher finisher
   #:early-finisher early-finisher
   #:name name))


(define-object-type branchless-reducer (starter consumer finisher)
  #:constructor-name constructor:branchless-reducer)


(define (make-branchless-reducer #:starter starter*
                                 #:consumer consumer*
                                 #:finisher finisher*
                                 #:name [name #false])
  (define starter
    (if (equal? (procedure-arity starter*) 0) starter* (procedure-reduce-arity starter* 0)))
  (define consumer
    (if (equal? (procedure-arity consumer*) 2) consumer* (procedure-reduce-arity consumer* 2)))
  (define finisher
    (if (equal? (procedure-arity finisher*) 1) finisher* (procedure-reduce-arity finisher* 1)))
  (constructor:branchless-reducer
   #:starter starter
   #:consumer consumer
   #:finisher finisher
   #:name name))


(define (branchless-reducer->generic-reducer reducer)
  (define branchless-starter (branchless-reducer-starter reducer))
  (define branchless-consumer (branchless-reducer-consumer reducer))
  (define branchless-finisher (branchless-reducer-finisher reducer))

  (define (start)
    (variant #:consume (branchless-starter)))
  
  (define (consume state v)
    (variant #:consume (branchless-consumer state v)))

  (make-reducer
   #:starter start
   #:consumer consume
   #:finisher branchless-finisher
   #:early-finisher void
   #:name (object-name reducer)))


(define (reducer? v)
  (or (generic-reducer? v)
      (branchless-reducer? v)))


(define/guard (reducer-starter reducer)
  (guard (generic-reducer? reducer) then
    (generic-reducer-starter reducer))
  (define branchless-starter (branchless-reducer-starter reducer))
  (define (start)
    (variant #:consume (branchless-starter)))
  start)


(define/guard (reducer-consumer reducer)
  (guard (generic-reducer? reducer) then
    (generic-reducer-consumer reducer))
  (define branchless-consumer (branchless-reducer-consumer reducer))
  (define (consume state v)
    (variant #:consume (branchless-consumer state v)))
  consume)


(define (reducer-finisher reducer)
  (if (generic-reducer? reducer)
      (generic-reducer-finisher reducer)
      (branchless-reducer-finisher reducer)))


(define (reducer-early-finisher reducer)
  (if (generic-reducer? reducer)
      (generic-reducer-early-finisher reducer)
      void))


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
  (if (generic-reducer? reducer)
      (generic-reducer-impersonate
       reducer
       #:domain-guard domain-guard
       #:range-guard range-guard
       #:properties properties
       #:chaperone? chaperone?)
      (branchless-reducer-impersonate
       reducer
       #:domain-guard domain-guard
       #:range-guard range-guard
       #:properties properties
       #:chaperone? chaperone?)))


(define (generic-reducer-impersonate
         reducer
         #:domain-guard [domain-guard #false]
         #:range-guard [range-guard #false]
         #:properties [properties (hash)]
         #:chaperone?
         [chaperone? (and (false? domain-guard) (false? range-guard))])
  (define consumer (generic-reducer-consumer reducer))
  (define finisher (generic-reducer-finisher reducer))
  (define early-finisher (generic-reducer-early-finisher reducer))
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
    (make-reducer #:starter (generic-reducer-starter reducer)
                  #:consumer impersonated-consumer
                  #:finisher impersonated-finisher
                  #:early-finisher impersonated-early-finisher
                  #:name (object-name reducer)))
  
  (object-impersonate impersonated-without-props descriptor:generic-reducer
                      #:properties properties))


(define (branchless-reducer-impersonate
         reducer
         #:domain-guard [domain-guard #false]
         #:range-guard [range-guard #false]
         #:properties [properties (hash)]
         #:chaperone?
         [chaperone? (and (false? domain-guard) (false? range-guard))])
  (define consumer (branchless-reducer-consumer reducer))
  (define finisher (branchless-reducer-finisher reducer))
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
  
  (define impersonated-without-props
    (make-branchless-reducer
     #:starter (branchless-reducer-starter reducer)
     #:consumer impersonated-consumer
     #:finisher impersonated-finisher
     #:name (object-name reducer)))
  
  (object-impersonate impersonated-without-props descriptor:branchless-reducer
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
