#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [sorted-submap-contains-key? (-> sorted-map? range? any/c boolean?)]
  [sorted-submap-contains-entry? (-> sorted-map? range? entry? boolean?)]
  [sorted-submap-get (-> sorted-map? range? any/c failure-result/c any/c)]
  [sorted-submap-get-option (-> sorted-map? range? any/c option?)]
  [sorted-submap-get-entry (-> sorted-map? range? any/c failure-result/c entry?)]
  [sorted-submap-least-key (-> sorted-map? range? option?)]
  [sorted-submap-greatest-key (-> sorted-map? range? option?)]
  [sorted-submap-key-less-than (-> sorted-map? range? any/c option?)]
  [sorted-submap-key-greater-than (-> sorted-map? range? any/c option?)]
  [sorted-submap-key-at-most (-> sorted-map? range? any/c option?)]
  [sorted-submap-key-at-least (-> sorted-map? range? any/c option?)]
  [sorted-submap-least-entry (-> sorted-map? range? (option/c entry?))]
  [sorted-submap-greatest-entry (-> sorted-map? range? (option/c entry?))]
  [sorted-submap-entry-less-than (-> sorted-map? range? any/c (option/c entry?))]
  [sorted-submap-entry-greater-than (-> sorted-map? range? any/c (option/c entry?))]
  [sorted-submap-entry-at-most (-> sorted-map? range? any/c (option/c entry?))]
  [sorted-submap-entry-at-least (-> sorted-map? range? any/c (option/c entry?))]))


(require racket/contract/combinator
         racket/match
         rebellion/base/comparator
         rebellion/base/option
         rebellion/base/range
         (submod rebellion/base/range private-for-rebellion-only)
         rebellion/collection/entry
         rebellion/collection/private/sorted-map-interface
         rebellion/private/cut
         guard)


;@----------------------------------------------------------------------------------------------------


(define (sorted-submap-contains-key? map key-range key)
  (define key-cmp (sorted-map-key-comparator map))
  (and (contract-first-order-passes? (comparator-operand-contract key-cmp) key)
       (range-contains? key-range key)
       (sorted-map-contains-key? map key)))


(define (sorted-submap-contains-entry? map key-range e)
  (match-define (entry key value) e)
  (define key-cmp (sorted-map-key-comparator map))
  (and (contract-first-order-passes? (comparator-operand-contract key-cmp) key)
       (range-contains? key-range key)
       (match (sorted-map-get-option map key)
         [(== absent) #false]
         [(present v) (equal? v value)])))


(define/guard (sorted-submap-get map key-range key failure-result)
  (define key-cmp (sorted-map-key-comparator map))
  (guard (and (contract-first-order-passes? (comparator-operand-contract key-cmp) key)
              (range-contains? key-range key)) #:else
    (if (procedure? failure-result) (failure-result) failure-result))
  (sorted-map-get map key failure-result))


(define/guard (sorted-submap-get-option map key-range key)
  (define key-cmp (sorted-map-key-comparator map))
  (guard (and (contract-first-order-passes? (comparator-operand-contract key-cmp) key)
              (range-contains? key-range key)) #:else
    absent)
  (sorted-map-get-option map key))


(define (sorted-submap-get-entry map key-range key failure-result)
  (entry key (sorted-submap-get map key-range key failure-result)))


(define/guard (sorted-submap-least-key map key-range)
  (define lower (range-lower-bound key-range))
  (guard (not (equal? lower unbounded)) #:else
    (sorted-map-least-key map))
  (define endpoint (range-bound-endpoint lower))
  (match (range-bound-type lower)
    [(== inclusive) (sorted-map-key-at-least map endpoint)]
    [(== exclusive) (sorted-map-key-greater-than map endpoint)]))


(define/guard (sorted-submap-least-entry map key-range)
  (define lower (range-lower-bound key-range))
  (guard (not (equal? lower unbounded)) #:else
    (sorted-map-least-entry map))
  (define endpoint (range-bound-endpoint lower))
  (match (range-bound-type lower)
    [(== inclusive) (sorted-map-entry-at-least map endpoint)]
    [(== exclusive) (sorted-map-entry-greater-than map endpoint)]))


(define/guard (sorted-submap-greatest-key map key-range)
  (define upper (range-upper-bound key-range))
  (guard (not (equal? upper unbounded)) #:else
    (sorted-map-greatest-key map))
  (define endpoint (range-bound-endpoint upper))
  (match (range-bound-type upper)
    [(== inclusive) (sorted-map-key-at-most map endpoint)]
    [(== exclusive) (sorted-map-key-less-than map endpoint)]))


(define/guard (sorted-submap-greatest-entry map key-range)
  (define upper (range-upper-bound key-range))
  (guard (not (equal? upper unbounded)) #:else
    (sorted-map-greatest-entry map))
  (define endpoint (range-bound-endpoint upper))
  (match (range-bound-type upper)
    [(== inclusive) (sorted-map-entry-at-most map endpoint)]
    [(== exclusive) (sorted-map-entry-less-than map endpoint)]))


(define (sorted-submap-key-less-than map key-range upper-bound)
  (match (range-compare-to-cut key-range (lower-cut upper-bound))
    [(== greater) (sorted-submap-greatest-key map key-range)]
    [(== lesser) absent]
    [(== equivalent)
     (define result (sorted-map-key-less-than map upper-bound))
     (option-filter result (λ (key) (range-contains? key-range key)))]))


(define (sorted-submap-entry-less-than map key-range upper-bound)
  (match (range-compare-to-cut key-range (lower-cut upper-bound))
    [(== greater) (sorted-submap-greatest-entry map key-range)]
    [(== lesser) absent]
    [(== equivalent)
     (define result (sorted-map-entry-less-than map upper-bound))
     (option-filter result (λ (e) (range-contains? key-range (entry-key e))))]))


(define (sorted-submap-key-greater-than map key-range lower-bound)
  (match (range-compare-to-cut key-range (upper-cut lower-bound))
    [(== lesser) (sorted-submap-least-key map key-range)]
    [(== greater) absent]
    [(== equivalent)
     (define result (sorted-map-key-greater-than map lower-bound))
     (option-filter result (λ (key) (range-contains? key-range key)))]))


(define (sorted-submap-entry-greater-than map key-range lower-bound)
  (match (range-compare-to-cut key-range (upper-cut lower-bound))
    [(== lesser) (sorted-submap-least-entry map key-range)]
    [(== greater) absent]
    [(== equivalent)
     (define result (sorted-map-entry-greater-than map lower-bound))
     (option-filter result (λ (e) (range-contains? key-range (entry-key e))))]))


(define (sorted-submap-key-at-most map key-range upper-bound)
  (match (range-compare-to-cut key-range (upper-cut upper-bound))
    [(== greater) (sorted-submap-greatest-key map key-range)]
    [(== lesser) absent]
    [(== equivalent)
     (define result (sorted-map-key-at-most map upper-bound))
     (option-filter result (λ (key) (range-contains? key-range key)))]))


(define (sorted-submap-entry-at-most map key-range upper-bound)
  (match (range-compare-to-cut key-range (upper-cut upper-bound))
    [(== greater) (sorted-submap-greatest-entry map key-range)]
    [(== lesser) absent]
    [(== equivalent)
     (define result (sorted-map-entry-at-most map upper-bound))
     (option-filter result (λ (e) (range-contains? key-range (entry-key e))))]))


(define (sorted-submap-key-at-least map key-range lower-bound)
  (match (range-compare-to-cut key-range (lower-cut lower-bound))
    [(== lesser) (sorted-submap-least-key map key-range)]
    [(== greater) absent]
    [(== equivalent)
     (define result (sorted-map-key-at-least map lower-bound))
     (option-filter result (λ (key) (range-contains? key-range key)))]))


(define (sorted-submap-entry-at-least map key-range lower-bound)
  (match (range-compare-to-cut key-range (lower-cut lower-bound))
    [(== lesser) (sorted-submap-least-entry map key-range)]
    [(== greater) absent]
    [(== equivalent)
     (define result (sorted-map-entry-at-least map lower-bound))
     (option-filter result (λ (e) (range-contains? key-range (entry-key e))))]))
