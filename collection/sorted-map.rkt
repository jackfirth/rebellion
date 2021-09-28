#lang racket/base


(require racket/contract/base)


(provide
 (all-from-out rebellion/collection/private/sorted-map-builder)
 (all-from-out rebellion/collection/private/sorted-map-interface)
 (contract-out
  [sorted-map (-> #:key-comparator comparator? any/c ... immutable-sorted-map?)]
  [entry-sequence->sorted-map
   (-> (sequence/c entry?) #:key-comparator comparator? immutable-sorted-map?)]
  [into-sorted-map (-> comparator? (reducer/c entry? immutable-sorted-map?))]))


(require racket/match
         racket/sequence
         rebellion/base/comparator
         rebellion/collection/entry
         rebellion/collection/private/sorted-map-builder
         rebellion/collection/private/sorted-map-interface
         rebellion/private/static-name
         rebellion/streaming/reducer)


(module+ test
  (require (submod "..")
           rackunit
           rebellion/streaming/transducer))


;@----------------------------------------------------------------------------------------------------


(define (sorted-map #:key-comparator key-comparator . entries)
  (for/fold ([builder (make-sorted-map-builder key-comparator)] #:result (build-sorted-map builder))
            ([e (in-slice 2 entries)])
    (match-define (list key value) e)
    (sorted-map-builder-put builder key value)))


(define (entry-sequence->sorted-map entries #:key-comparator key-comparator)
  (for/fold ([builder (make-sorted-map-builder key-comparator)] #:result (build-sorted-map builder))
            ([e entries])
    (match-define (entry key value) e)
    (sorted-map-builder-put builder key value)))


(define (into-sorted-map key-comparator)
  (make-effectful-fold-reducer
   (λ (builder e) (sorted-map-builder-put builder (entry-key e) (entry-value e)))
   (λ () (make-sorted-map-builder key-comparator))
   build-sorted-map
   #:name (name into-sorted-map)))


(module+ test

  (test-case (name-string sorted-map)

    (check-equal?
     (sorted-map 1 'a 2 'b 3 'c #:key-comparator natural<=>)
     (sorted-map 1 'a 2 'b 3 'c #:key-comparator natural<=>))

    (test-case "duplicate key detection"
      (check-exn exn:fail:contract? (λ () (sorted-map 1 'a 1 'b #:key-comparator natural<=>)))))

  (test-case (name-string entry-sequence->sorted-map)

    (check-equal?
     (entry-sequence->sorted-map
      (list (entry 1 'a) (entry 2 'b) (entry 3 'c))
      #:key-comparator natural<=>)
     (sorted-map 1 'a 2 'b 3 'c #:key-comparator natural<=>))

    (test-case "duplicate key detection"
      (check-exn
       exn:fail:contract?
       (λ ()
         (entry-sequence->sorted-map (list (entry 1 'a) (entry 1 'b)) #:key-comparator natural<=>)))))

  (test-case (name-string into-sorted-map)

    (check-equal?
     (transduce (list (entry 1 'a) (entry 2 'b) (entry 3 'c)) #:into (into-sorted-map natural<=>))
     (sorted-map 1 'a 2 'b 3 'c #:key-comparator natural<=>))

    (test-case "duplicate key detection"
      (check-exn
       exn:fail:contract?
       (λ () (transduce (list (entry 1 'a) (entry 1 'b)) #:into (into-sorted-map natural<=>)))))))
