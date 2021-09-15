#lang racket/base


(require racket/contract/base)


(provide
 (struct-out position)
 (struct-out gap)
 (contract-out
  [vector-binary-search
   (->* (vector? any/c #:comparator comparator?) (natural? natural?) (or/c position? gap?))]
  [vector-binary-search-cut
   (->* (vector? cut? #:comparator comparator?) (natural? natural?) (or/c position? gap?))]
  [vector-binary-search-element-less-than
   (->* (vector? any/c #:comparator comparator?) (natural? natural?) option?)]
  [vector-binary-search-element-greater-than
   (->* (vector? any/c #:comparator comparator?) (natural? natural?) option?)]
  [vector-binary-search-element-at-most
   (->* (vector? any/c #:comparator comparator?) (natural? natural?) option?)]
  [vector-binary-search-element-at-least
   (->* (vector? any/c #:comparator comparator?) (natural? natural?) option?)]
  [range-vector-binary-search (->* (vector? any/c) (natural? natural?) (or/c position? gap?))]
  [range-vector-binary-search-cut (->* (vector? cut?) (natural? natural?) (or/c position? gap?))]))


(require racket/match
         racket/math
         rebellion/base/comparator
         rebellion/base/option
         rebellion/base/range
         (submod rebellion/base/range private-for-rebellion-only)
         rebellion/private/cut
         rebellion/private/guarded-block
         rebellion/private/static-name)


(module+ test
  (require (submod "..")
           rackunit))


;@----------------------------------------------------------------------------------------------------


(struct position (index element)
  #:transparent
  #:guard (struct-guard/c natural? any/c))


;; The gap index indicates where in the vector the search ended up. It's not a normal vector index,
;; it's a position *between* vector indices, so its range is [0, vector-length + 1] inclusive.
(struct gap (index element-before element-after)
  #:transparent
  #:guard (struct-guard/c natural? option? option?))


;; Searches vec for an element for which the search function returns the `equivalent` constant, then
;; returns the index of that element if such an element exists. The vector is assumed to be sorted in
;; a manner consistent with the comparison results returned by the search function. The search is a
;; binary search taking log(n) time. Note that if there are multiple elements for which the search
;; function returns `equivalent`, it is unspecified which of them is chosen. If no elements satisfy
;; the search function, `absent` is returned.
;; Examples:
;; > (vector-generalized-binary-search (vector "a" "b" "d" "e") (λ (x) (compare string<=> x "d")))
;; (exact-match 2)
;; > (vector-generalized-binary-search (vector "a" "b" "d" "e") (λ (x) (compare string<=> x "c")))
;; (no-match 2)
(define (vector-generalized-binary-search vec search-function [start 0] [end (vector-length vec)])

  (define/guard (loop [lower start]
                      [lower-element absent]
                      [upper (sub1 end)]
                      [upper-element absent])
    (guard (<= lower upper) else
      (gap lower lower-element upper-element))
    (define middle (quotient (+ lower upper) 2))
    (define middle-element (vector-ref vec middle))
    (match (search-function middle-element)
      [(== lesser) (loop (add1 middle) (present middle-element) upper upper-element)]
      [(== greater) (loop lower lower-element (sub1 middle) (present middle-element))]
      [(== equivalent) (position middle middle-element)]))
  
  (loop))


(define (vector-binary-search vec element #:comparator cmp [start 0] [end (vector-length vec)])
  (vector-generalized-binary-search vec (λ (x) (compare cmp x element)) start end))


(module+ test
  (test-case (name-string vector-binary-search)
    (check-equal? (vector-binary-search (vector) "a" #:comparator string<=>) (gap 0 absent absent))
    (define vec (vector "aa" "bb" "cc" "dd"))
    (check-equal? (vector-binary-search vec "aa" #:comparator string<=>) (position 0 "aa"))
    (check-equal? (vector-binary-search vec "bb" #:comparator string<=>) (position 1 "bb"))
    (check-equal? (vector-binary-search vec "cc" #:comparator string<=>) (position 2 "cc"))
    (check-equal? (vector-binary-search vec "dd" #:comparator string<=>) (position 3 "dd"))
    (check-equal? (vector-binary-search vec "a" #:comparator string<=>) (gap 0 absent (present "aa")))
    (check-equal?
     (vector-binary-search vec "b" #:comparator string<=>)
     (gap 1 (present "aa") (present "bb")))
    (check-equal?
     (vector-binary-search vec "c" #:comparator string<=>)
     (gap 2 (present "bb") (present "cc")))
    (check-equal?
     (vector-binary-search vec "d" #:comparator string<=>)
     (gap 3 (present "cc") (present "dd")))
    (check-equal?
     (vector-binary-search vec "e" #:comparator string<=>)
     (gap 4 (present "dd") absent))))


(define (vector-binary-search-cut vec cut #:comparator cmp [start 0] [end (vector-length vec)])
  (define cut-cmp (cut<=> cmp))
  (vector-generalized-binary-search vec (λ (c) (compare cut-cmp (middle-cut c) cut)) start end))


(module+ test
  (test-case (name-string vector-binary-search-cut)

    (check-equal?
     (vector-binary-search-cut (vector) (middle-cut "a") #:comparator string<=>)
     (gap 0 absent absent))
             
    (define vec (vector "aa" "bb" "cc" "dd"))

    (define (search c)
      (vector-binary-search-cut vec c #:comparator string<=>))
    
    (check-equal? (search bottom-cut) (gap 0 absent (present "aa")))

    (check-equal? (search (lower-cut "a")) (gap 0 absent (present "aa")))
    (check-equal? (search (middle-cut "a")) (gap 0 absent (present "aa")))
    (check-equal? (search (upper-cut "a")) (gap 0 absent (present "aa")))

    (check-equal? (search (lower-cut "aa")) (gap 0 absent (present "aa")))
    (check-equal? (search (middle-cut "aa")) (position 0 "aa"))
    (check-equal? (search (upper-cut "aa")) (gap 1 (present "aa") (present "bb")))

    (check-equal? (search (lower-cut "b")) (gap 1 (present "aa") (present "bb")))
    (check-equal? (search (middle-cut "b")) (gap 1 (present "aa") (present "bb")))
    (check-equal? (search (upper-cut "b")) (gap 1 (present "aa") (present "bb")))

    (check-equal? (search (lower-cut "bb")) (gap 1 (present "aa") (present "bb")))
    (check-equal? (search (middle-cut "bb")) (position 1 "bb"))
    (check-equal? (search (upper-cut "bb")) (gap 2 (present "bb") (present "cc")))

    (check-equal? (search (lower-cut "c")) (gap 2 (present "bb") (present "cc")))
    (check-equal? (search (middle-cut "c")) (gap 2 (present "bb") (present "cc")))
    (check-equal? (search (upper-cut "c")) (gap 2 (present "bb") (present "cc")))

    (check-equal? (search (lower-cut "cc")) (gap 2 (present "bb") (present "cc")))
    (check-equal? (search (middle-cut "cc")) (position 2 "cc"))
    (check-equal? (search (upper-cut "cc")) (gap 3 (present "cc") (present "dd")))

    (check-equal? (search (lower-cut "d")) (gap 3 (present "cc") (present "dd")))
    (check-equal? (search (middle-cut "d")) (gap 3 (present "cc") (present "dd")))
    (check-equal? (search (upper-cut "d")) (gap 3 (present "cc") (present "dd")))

    (check-equal? (search (lower-cut "dd")) (gap 3 (present "cc") (present "dd")))
    (check-equal? (search (middle-cut "dd")) (position 3 "dd"))
    (check-equal? (search (upper-cut "dd")) (gap 4 (present "dd") absent))

    (check-equal? (search (lower-cut "e")) (gap 4 (present "dd") absent))
    (check-equal? (search (middle-cut "e")) (gap 4 (present "dd") absent))
    (check-equal? (search (upper-cut "e")) (gap 4 (present "dd") absent))

    (check-equal? (search top-cut) (gap 4 (present "dd") absent))))


(define (vector-binary-search-element-less-than
         vec upper-bound [start 0] [end (vector-length vec)] #:comparator cmp)
  (gap-element-before
   (vector-binary-search-cut vec (lower-cut upper-bound) start end #:comparator cmp)))


(define (vector-binary-search-element-greater-than
         vec lower-bound [start 0] [end (vector-length vec)] #:comparator cmp)
  (gap-element-after
   (vector-binary-search-cut vec (upper-cut lower-bound) start end #:comparator cmp)))


(define (vector-binary-search-element-at-most
         vec upper-bound [start 0] [end (vector-length vec)] #:comparator cmp)
  (gap-element-before
   (vector-binary-search-cut vec (upper-cut upper-bound) start end #:comparator cmp)))


(define (vector-binary-search-element-at-least
         vec lower-bound [start 0] [end (vector-length vec)] #:comparator cmp)
  (gap-element-after
   (vector-binary-search-cut vec (lower-cut lower-bound) start end #:comparator cmp)))


(define (range-vector-binary-search range-vec value [start 0] [end (vector-length range-vec)])
  (vector-generalized-binary-search range-vec (λ (r) (range-compare-to-value r value)) start end))


(define (range-vector-binary-search-cut range-vec cut [start 0] [end (vector-length range-vec)])
  (vector-generalized-binary-search range-vec (λ (r) (range-compare-to-cut r cut)) start end))
