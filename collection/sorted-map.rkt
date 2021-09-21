#lang racket/base


(require racket/contract/base)


(provide
 (all-from-out rebellion/collection/private/sorted-map-interface)
 (contract-out
  [sorted-map (-> #:key-comparator comparator? any/c ... immutable-sorted-map?)]))


(require racket/match
         racket/sequence
         rebellion/base/comparator
         rebellion/base/result
         rebellion/collection/private/sorted-map-interface
         (submod rebellion/collection/private/persistent-sorted-map private-for-rebellion-only)
         rebellion/private/static-name)


(module+ test
  (require (submod "..")
           rackunit))


;@----------------------------------------------------------------------------------------------------


(define (sorted-map #:key-comparator key-comparator . entries)
  (for/fold ([map (empty-sorted-map key-comparator)])
            ([e (in-slice 2 entries)])
    (match-define (list key value) e)
    (match (sorted-map-put-if-absent map key value)
      [(success new-map) new-map]
      [(failure previous-value)
       (raise-arguments-error
        (name sorted-map)
        "duplicate keys not allowed"
        "key" key
        "value1" previous-value
        "value2" value)])))


(module+ test

  (test-case (name-string sorted-map)

    (check-equal?
     (sorted-map 1 'a 2 'b 3 'c #:key-comparator natural<=>)
     (sorted-map 1 'a 2 'b 3 'c #:key-comparator natural<=>))

    (test-case "duplicate key detection"
      (check-exn exn:fail:contract? (λ () (sorted-map 1 'a 1 'b #:key-comparator natural<=>))))))
