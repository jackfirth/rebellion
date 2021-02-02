#lang racket/base


(module+ test
  (require racket/contract/base
           racket/contract/combinator
           racket/contract/region
           rackunit
           rebellion/collection/list
           rebellion/private/static-name
           rebellion/streaming/reducer))


;@----------------------------------------------------------------------------------------------------


(module+ test
  (test-case (name-string reducer-impersonate)
    (test-case "properties only"
      (define reducer (into-all-match? even?))
      (define properties (hash impersonator-prop:contracted 'foo))
      (define impersonated
        (reducer-impersonate reducer #:properties properties))
      (check-equal? (value-contract impersonated) 'foo)
      (check-equal? impersonated reducer)
      (check impersonator-of? impersonated reducer)
      (check impersonator-of? reducer impersonated)
      (check chaperone-of? impersonated reducer)
      (check chaperone-of? reducer impersonated))
    
    (test-case "domain guard"
      (define counter (box 0))
      (define (guard v)
        (set-box! counter (add1 (unbox counter)))
        v)
      (define impersonated
        (reducer-impersonate into-list #:domain-guard guard #:chaperone? #true))
      (check-equal? (reduce impersonated 'a 'b 'c) (list 'a 'b 'c))
      (check-equal? (unbox counter) 3)
      (check-equal? impersonated into-list)
      (check impersonator-of? impersonated into-list)
      (check-false (impersonator-of? into-list impersonated))
      (check chaperone-of? impersonated into-list)
      (check-false (chaperone-of? into-list impersonated)))

    (test-case "range guard"
      (define result (box #false))
      (define (guard v)
        (set-box! result v)
        v)
      (define impersonated
        (reducer-impersonate into-list #:range-guard guard #:chaperone? #true))
      (check-equal? (reduce impersonated 1 2 3) (list 1 2 3))
      (check-equal? (unbox result) (list 1 2 3))
      (check-equal? impersonated into-list)
      (check impersonator-of? impersonated into-list)
      (check-false (impersonator-of? into-list impersonated))
      (check chaperone-of? impersonated into-list)
      (check-false (chaperone-of? into-list impersonated))))

  (test-case (name-string reducer/c)
    (test-case "should enforce the domain contract on sequence elements"
      (define/contract reducer (reducer/c number? any/c) into-list)
      (check-not-exn (λ () (reduce reducer 1 2 3)))
      (define (bad) (reduce reducer 1 2 'foo 3))
      (check-exn exn:fail:contract:blame? bad)
      (check-exn #rx"expected: number\\?" bad)
      (check-exn #rx"given: 'foo" bad)
      (check-exn #rx"an element reduced by" bad))

    (test-case "should enforce the range contract on reduction results"
      (define/contract reducer (reducer/c any/c integer?) into-sum)
      (check-not-exn (λ () (reduce reducer 1 2 3)))
      (define (bad) (reduce reducer 1 2 3.5))
      (check-exn exn:fail:contract:blame? bad)
      (check-exn #rx"promised: integer\\?" bad)
      (check-exn #rx"produced: 6\\.5" bad)
      (check-exn #rx"the reduction result of" bad))))