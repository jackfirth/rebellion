#lang racket/base


(require racket/contract/base)


(provide
 (all-from-out rebellion/collection/private/sorted-map-builder)
 (all-from-out rebellion/collection/private/sorted-map-interface)
 (contract-out
  [sorted-map (-> #:key-comparator comparator? any/c ... immutable-sorted-map?)]))


(require racket/match
         racket/sequence
         rebellion/base/comparator
         rebellion/base/result
         rebellion/collection/private/sorted-map-builder
         rebellion/collection/private/sorted-map-interface
         (submod rebellion/collection/private/persistent-sorted-map private-for-rebellion-only)
         rebellion/private/static-name)


(module+ test
  (require (submod "..")
           rackunit))


;@----------------------------------------------------------------------------------------------------


(define (sorted-map #:key-comparator key-comparator . entries)
  (for/fold ([builder (make-sorted-map-builder key-comparator)] #:result (build-sorted-map builder))
            ([e (in-slice 2 entries)])
    (match-define (list key value) e)
    (sorted-map-builder-put builder key value)))


(module+ test

  (test-case (name-string sorted-map)

    (check-equal?
     (sorted-map 1 'a 2 'b 3 'c #:key-comparator natural<=>)
     (sorted-map 1 'a 2 'b 3 'c #:key-comparator natural<=>))

    (test-case "duplicate key detection"
      (check-exn exn:fail:contract? (λ () (sorted-map 1 'a 1 'b #:key-comparator natural<=>))))))
