#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [vector-merge-adjacent
   (-> vector? (-> any/c any/c boolean?) (-> any/c any/c any/c) (and/c vector? immutable?))]))


(require guard
         rebellion/collection/vector/builder)


(module+ test
  (require (submod "..")
           rackunit
           rebellion/private/static-name))


;@----------------------------------------------------------------------------------------------------


;; Returns a new (immutable) vector that is like vec, except adjacent elements are merged with
;; merge-function when should-merge? returns true.
;;
;; Examples:
;;
;; > (vector-merge-adjacent (vector 1 2 3 "hello" 4 5 "world" 6) both-numbers? +)
;; (vector 6 "hello" 9 "world" 6)
;;
(define/guard (vector-merge-adjacent vec should-merge? merge-function)
  (define count (vector-length vec))
  (guard (>= count 2) #:else
    (vector->immutable-vector vec))
  (for/fold ([builder (make-vector-builder #:expected-size count)]
             [element (vector-ref vec 0)]
             #:result (build-vector (vector-builder-add builder element)))
            ([next-element (in-vector vec 1)])
    (if (should-merge? element next-element)
        (values builder (merge-function element next-element))
        (values (vector-builder-add builder element) next-element))))


(module+ test
  (test-case (name-string vector-merge-adjacent)

    (define (both-numbers? left right)
      (and (number? left) (number? right)))

    (define (fail-immediately left right)
      (raise 'should-not-be-called))

    (test-case "empty vectors are returned uninspected"
      (define actual (vector-merge-adjacent (vector-immutable) fail-immediately fail-immediately))
      (check-equal? actual (vector-immutable)))

    (test-case "single-element vectors are returned uninspected"
      (define actual (vector-merge-adjacent (vector-immutable 1) fail-immediately fail-immediately))
      (check-equal? actual (vector-immutable 1)))

    (test-case "can merge all elements"
      (define actual (vector-merge-adjacent (vector-immutable 1 2 3 4 5) (λ (a b) #true) +))
      (check-equal? actual (vector-immutable 15)))

    (test-case "can merge no elements"
      (define actual
        (vector-merge-adjacent (vector-immutable 1 2 3 4 5) (λ (a b) #false) fail-immediately))
      (check-equal? actual (vector-immutable 1 2 3 4 5)))

    (test-case "can merge elements at start"
      (define actual (vector-merge-adjacent (vector-immutable 1 2 3 'a 'b 'c) both-numbers? +))
      (check-equal? actual (vector-immutable 6 'a 'b 'c)))

    (test-case "can merge elements at end"
      (define actual (vector-merge-adjacent (vector-immutable 'a 'b 'c 4 5 6) both-numbers? +))
      (check-equal? actual (vector-immutable 'a 'b 'c 15)))

    (test-case "can merge elements in middle"
      (define actual
        (vector-merge-adjacent (vector-immutable 'a 'b 'c 4 5 6 'd 'e 'f) both-numbers? +))
      (check-equal? actual (vector-immutable 'a 'b 'c 15 'd 'e 'f)))

    (test-case "can merge elements in middle multiple times"
      (define actual
        (vector-merge-adjacent (vector-immutable 1 2 3 "hello" 4 5 "world" 6) both-numbers? +))
      (check-equal? actual (vector-immutable 6 "hello" 9 "world" 6)))))