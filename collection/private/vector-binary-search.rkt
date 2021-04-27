#lang racket/base


(require racket/contract/base)


(provide
 exact-match
 no-match
 (contract-out
  [exact-match? predicate/c]
  [exact-match-index (-> exact-match? natural?)]
  [no-match? predicate/c]
  [no-match-position (-> no-match? natural?)]
  [vector-binary-search (-> vector? (-> any/c comparison?) (or/c exact-match? no-match?))]))


(require racket/match
         racket/math
         rebellion/base/comparator
         rebellion/private/guarded-block
         rebellion/private/static-name
         rebellion/type/tuple)


(module+ test
  (require (submod "..")
           rackunit))


;@----------------------------------------------------------------------------------------------------


(define-tuple-type exact-match (index))

;; The position indicates where in the vector the search ended up. It's not an index, it's a position
;; *between* indices, so its range is [0, vector-length + 1] inclusive.
(define-tuple-type no-match (position))

;; Vector A, (A -> Comparison) -> (Or ExactMatch NoMatch)
;; Searches vec for an element for which the search function returns the `equivalent` constant, then
;; returns the index of that element if such an element exists. The vector is assumed to be sorted in
;; a manner consistent with the comparison results returned by the search function. The search is a
;; binary search taking log(n) time. Note that if there are multiple elements for which the search
;; function returns `equivalent`, it is unspecified which of them is chosen. If no elements satisfy
;; the search function, `absent` is returned.
;; Examples:
;; > (vector-binary-search (vector "a" "b" "d" "e") (λ (x) (compare string<=> x "d")))
;; (exact-match 2)
;; > (vector-binary-search (vector "a" "b" "d" "e") (λ (x) (compare string<=> x "c")))
;; (no-match 2)
(define (vector-binary-search vec search-function)
  (define/guard (loop [lower 0] [upper (sub1 (vector-length vec))])
    (guard (<= lower upper) else
      (no-match lower))
    (define middle (quotient (+ lower upper) 2))
    (match (search-function (vector-ref vec middle))
      [(== lesser) (loop (add1 middle) upper)]
      [(== greater) (loop lower (sub1 middle))]
      [(== equivalent) (exact-match middle)]))
  (loop))


(module+ test
  (test-case (name-string vector-binary-search)
    (define vec (vector "aa" "bb" "cc" "dd"))
    (check-equal? (vector-binary-search vec (λ (x) (compare string<=> x "aa"))) (exact-match 0))
    (check-equal? (vector-binary-search vec (λ (x) (compare string<=> x "bb"))) (exact-match 1))
    (check-equal? (vector-binary-search vec (λ (x) (compare string<=> x "cc"))) (exact-match 2))
    (check-equal? (vector-binary-search vec (λ (x) (compare string<=> x "dd"))) (exact-match 3))
    (check-equal? (vector-binary-search vec (λ (x) (compare string<=> x "a"))) (no-match 0))
    (check-equal? (vector-binary-search vec (λ (x) (compare string<=> x "b"))) (no-match 1))
    (check-equal? (vector-binary-search vec (λ (x) (compare string<=> x "c"))) (no-match 2))
    (check-equal? (vector-binary-search vec (λ (x) (compare string<=> x "d"))) (no-match 3))
    (check-equal? (vector-binary-search vec (λ (x) (compare string<=> x "e"))) (no-match 4))
    (check-equal? (vector-binary-search (vector) (λ (x) (compare string<=> x "a"))) (no-match 0))))
