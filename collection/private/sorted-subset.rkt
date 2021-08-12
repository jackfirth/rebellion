#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [sorted-subset-contains? (-> sorted-set? range? any/c boolean?)]
  [sorted-subset-least-element (-> sorted-set? range? option?)]
  [sorted-subset-greatest-element (-> sorted-set? range? option?)]
  [sorted-subset-element-less-than (-> sorted-set? range? any/c option?)]
  [sorted-subset-element-greater-than (-> sorted-set? range? any/c option?)]
  [sorted-subset-element-at-most (-> sorted-set? range? any/c option?)]
  [sorted-subset-element-at-least (-> sorted-set? range? any/c option?)]))


(require racket/match
         rebellion/base/comparator
         rebellion/base/option
         rebellion/base/range
         (submod rebellion/base/range private-for-rebellion-only)
         rebellion/collection/private/sorted-set-interface
         rebellion/private/cut
         rebellion/private/guarded-block)


;@----------------------------------------------------------------------------------------------------


(define (sorted-subset-contains? set element-range value)
  (and (range-contains? element-range value)
       (sorted-set-contains? set value)))


(define/guard (sorted-subset-least-element tree range)
  (define lower (range-lower-bound range))
  (guard (equal? lower unbounded) then
    (sorted-set-least-element tree))
  (define endpoint (range-bound-endpoint lower))
  (match (range-bound-type lower)
    [(== inclusive) (sorted-set-element-at-least tree endpoint)]
    [(== exclusive) (sorted-set-element-greater-than tree endpoint)]))


(define/guard (sorted-subset-greatest-element tree range)
  (define upper (range-upper-bound range))
  (guard (equal? upper unbounded) then
    (sorted-set-greatest-element tree))
  (define endpoint (range-bound-endpoint upper))
  (match (range-bound-type upper)
    [(== inclusive) (sorted-set-element-at-most tree endpoint)]
    [(== exclusive) (sorted-set-element-less-than tree endpoint)]))


(define (sorted-subset-element-less-than tree range upper-bound)
  (match (range-compare-to-cut range (lower-cut upper-bound))
    [(== greater) (sorted-subset-greatest-element tree range)]
    [(== lesser) absent]
    [(== equivalent)
     (define result (sorted-set-element-less-than tree upper-bound))
     (option-filter result (λ (element) (range-contains? range element)))]))


(define (sorted-subset-element-greater-than tree range lower-bound)
  (match (range-compare-to-cut range (upper-cut lower-bound))
    [(== lesser) (sorted-subset-least-element tree range)]
    [(== greater) absent]
    [(== equivalent)
     (define result (sorted-set-element-greater-than tree lower-bound))
     (option-filter result (λ (element) (range-contains? range element)))]))


(define (sorted-subset-element-at-most tree range upper-bound)
  (match (range-compare-to-cut range (upper-cut upper-bound))
    [(== greater) (sorted-subset-greatest-element tree range)]
    [(== lesser) absent]
    [(== equivalent)
     (define result (sorted-set-element-at-most tree upper-bound))
     (option-filter result (λ (element) (range-contains? range element)))]))


(define (sorted-subset-element-at-least tree range lower-bound)
  (match (range-compare-to-cut range (lower-cut lower-bound))
    [(== lesser) (sorted-subset-least-element tree range)]
    [(== greater) absent]
    [(== equivalent)
     (define result (sorted-set-element-at-least tree lower-bound))
     (option-filter result (λ (element) (range-contains? range element)))]))
