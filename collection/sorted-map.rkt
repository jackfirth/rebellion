#lang racket/base


(require racket/contract/base)


(provide
 (all-from-out rebellion/collection/private/sorted-map-interface)
 (contract-out
  [sorted-map (-> #:key-comparator comparator? any/c ... immutable-sorted-map?)]))


(require racket/match
         racket/sequence
         rebellion/base/comparator
         rebellion/collection/private/sorted-map-interface
         (submod rebellion/collection/private/persistent-sorted-map private-for-rebellion-only))


;@----------------------------------------------------------------------------------------------------


(define (sorted-map #:key-comparator key-comparator . entries)
  (for/fold ([map (empty-sorted-map key-comparator)])
            ([e (in-slice 2 entries)])
    (match-define (list key value) e)
    (sorted-map-put map key value)))
