#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [reducer? predicate/c]
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
  [reducer-early-finisher (-> reducer? (-> any/c any/c))]))


(module+ for-contracts
  (provide descriptor:reducer))


(require rebellion/base/symbol
         rebellion/base/variant
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
