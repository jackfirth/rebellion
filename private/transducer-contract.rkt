#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [transducer/c (-> contract? contract? contract?)]))

(require racket/contract/combinator
         rebellion/private/contract-projection
         rebellion/private/static-name
         rebellion/streaming/transducer/base)

;@------------------------------------------------------------------------------

(define/name (transducer/c domain-contract* range-contract*)
  (define domain-contract
    (coerce-contract enclosing-function-name domain-contract*))
  (define range-contract
    (coerce-contract enclosing-function-name range-contract*))
  (define contract-name
    (build-compound-type-name enclosing-function-name
                              domain-contract
                              range-contract))
  (define domain-projection (contract-late-neg-projection domain-contract))
  (define range-projection (contract-late-neg-projection range-contract))
  (define chaperone?
    (and (chaperone-contract? domain-contract)
         (chaperone-contract? range-contract)))
  (define (projection blame)
    (define domain-blame
      (blame-add-context blame "an element consumed by" #:swap? #t))
    (define range-blame (blame-add-context blame "an element emitted by"))
    (define late-neg-domain-guard (domain-projection domain-blame))
    (define late-neg-range-guard (range-projection range-blame))
    (λ (v missing-party)
      (assert-satisfies v transducer? blame #:missing-party missing-party)
      (define props
        (hash impersonator-prop:contracted the-contract
              impersonator-prop:blame (cons blame missing-party)))
      (define (domain-guard v) (late-neg-domain-guard v missing-party))
      (define (range-guard v) (late-neg-range-guard v missing-party))
      (transducer-impersonate v
                           #:domain-guard domain-guard
                           #:range-guard range-guard
                           #:chaperone? chaperone?
                           #:properties props)))
  (define the-contract
    ((if chaperone? make-chaperone-contract make-contract)
     #:name contract-name
     #:first-order transducer?
     #:late-neg-projection projection))
  the-contract)
