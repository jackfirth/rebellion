#lang racket/base


(require racket/contract/base)


(provide
 (struct-out list-position)
 (struct-out list-gap)
 (struct-out map-position)
 (struct-out map-gap)
 (contract-out
  [vector-binary-search
   (->* (vector? any/c #:comparator comparator?) (natural? natural?) (or/c list-position? list-gap?))]
  [vector-binary-search-cut
   (->* (vector? cut? #:comparator comparator?) (natural? natural?) (or/c list-position? list-gap?))]
  [vector-binary-search-element-less-than
   (->* (vector? any/c #:comparator comparator?) (natural? natural?) option?)]
  [vector-binary-search-element-greater-than
   (->* (vector? any/c #:comparator comparator?) (natural? natural?) option?)]
  [vector-binary-search-element-at-most
   (->* (vector? any/c #:comparator comparator?) (natural? natural?) option?)]
  [vector-binary-search-element-at-least
   (->* (vector? any/c #:comparator comparator?) (natural? natural?) option?)]
  [range-vector-binary-search
   (->* (vector? any/c) (natural? natural?) (or/c list-position? list-gap?))]
  [range-vector-binary-search-cut
   (->* (vector? cut?) (natural? natural?) (or/c list-position? list-gap?))]))


(require racket/match
         racket/math
         rebellion/base/comparator
         rebellion/base/option
         rebellion/base/range
         (submod rebellion/base/range private-for-rebellion-only)
         rebellion/collection/entry
         rebellion/private/cut
         rebellion/private/guarded-block
         rebellion/private/static-name)


(module+ test
  (require (submod "..")
           rackunit))


;@----------------------------------------------------------------------------------------------------


(struct list-position (index element)
  #:transparent
  #:guard (struct-guard/c natural? any/c))


;; The gap index indicates where in the vector the search ended up. It's not a normal vector index,
;; it's a position *between* vector indices, so its range is [0, vector-length + 1] inclusive.
(struct list-gap (index element-before element-after)
  #:transparent
  #:guard (struct-guard/c natural? option? option?))


(struct map-position (index key element)
  #:transparent
  #:guard (struct-guard/c natural? any/c any/c))


(struct map-gap (index entry-before entry-after)
  #:transparent
  #:guard (struct-guard/c natural? (option/c entry?) (option/c entry?)))


;; Searches vec for an element for which the search function returns the `equivalent` constant, then
;; returns the index of that element if such an element exists. The vector is assumed to be sorted in
;; a manner consistent with the comparison results returned by the search function. The search is a
;; binary search taking log(n) time. Note that if there are multiple elements for which the search
;; function returns `equivalent`, it is unspecified which of them is chosen. If no elements satisfy
;; the search function, `absent` is returned.
;; Examples:
;; > (vector-generalized-binary-search (vector "a" "b" "d" "e") (λ (x) (compare string<=> x "d")))
;; (list-position 2 "d")
;; > (vector-generalized-binary-search (vector "a" "b" "d" "e") (λ (x) (compare string<=> x "c")))
;; (list-gap 2 (present "b") (present "d"))
(define (vector-generalized-binary-search vec search-function [start 0] [end (vector-length vec)])

  (define/guard (loop [lower start]
                      [lower-element absent]
                      [upper (sub1 end)]
                      [upper-element absent])
    (guard (<= lower upper) else
      (list-gap lower lower-element upper-element))
    (define middle (quotient (+ lower upper) 2))
    (define middle-element (vector-ref vec middle))
    (match (search-function middle-element)
      [(== lesser) (loop (add1 middle) (present middle-element) upper upper-element)]
      [(== greater) (loop lower lower-element (sub1 middle) (present middle-element))]
      [(== equivalent) (list-position middle middle-element)]))
  
  (loop))


(define (vector-binary-search vec element #:comparator cmp [start 0] [end (vector-length vec)])
  (vector-generalized-binary-search vec (λ (x) (compare cmp x element)) start end))


(module+ test
  (test-case (name-string vector-binary-search)
    (check-equal?
     (vector-binary-search (vector) "a" #:comparator string<=>) (list-gap 0 absent absent))
    (define vec (vector "aa" "bb" "cc" "dd"))
    (check-equal? (vector-binary-search vec "aa" #:comparator string<=>) (list-position 0 "aa"))
    (check-equal? (vector-binary-search vec "bb" #:comparator string<=>) (list-position 1 "bb"))
    (check-equal? (vector-binary-search vec "cc" #:comparator string<=>) (list-position 2 "cc"))
    (check-equal? (vector-binary-search vec "dd" #:comparator string<=>) (list-position 3 "dd"))
    (check-equal?
     (vector-binary-search vec "a" #:comparator string<=>) (list-gap 0 absent (present "aa")))
    (check-equal?
     (vector-binary-search vec "b" #:comparator string<=>)
     (list-gap 1 (present "aa") (present "bb")))
    (check-equal?
     (vector-binary-search vec "c" #:comparator string<=>)
     (list-gap 2 (present "bb") (present "cc")))
    (check-equal?
     (vector-binary-search vec "d" #:comparator string<=>)
     (list-gap 3 (present "cc") (present "dd")))
    (check-equal?
     (vector-binary-search vec "e" #:comparator string<=>)
     (list-gap 4 (present "dd") absent))))


(define (vector-binary-search-cut vec cut #:comparator cmp [start 0] [end (vector-length vec)])
  (define cut-cmp (cut<=> cmp))
  (vector-generalized-binary-search vec (λ (c) (compare cut-cmp (middle-cut c) cut)) start end))


(module+ test
  (test-case (name-string vector-binary-search-cut)

    (check-equal?
     (vector-binary-search-cut (vector) (middle-cut "a") #:comparator string<=>)
     (list-gap 0 absent absent))
             
    (define vec (vector "aa" "bb" "cc" "dd"))

    (define (search c)
      (vector-binary-search-cut vec c #:comparator string<=>))
    
    (check-equal? (search bottom-cut) (list-gap 0 absent (present "aa")))

    (check-equal? (search (lower-cut "a")) (list-gap 0 absent (present "aa")))
    (check-equal? (search (middle-cut "a")) (list-gap 0 absent (present "aa")))
    (check-equal? (search (upper-cut "a")) (list-gap 0 absent (present "aa")))

    (check-equal? (search (lower-cut "aa")) (list-gap 0 absent (present "aa")))
    (check-equal? (search (middle-cut "aa")) (list-position 0 "aa"))
    (check-equal? (search (upper-cut "aa")) (list-gap 1 (present "aa") (present "bb")))

    (check-equal? (search (lower-cut "b")) (list-gap 1 (present "aa") (present "bb")))
    (check-equal? (search (middle-cut "b")) (list-gap 1 (present "aa") (present "bb")))
    (check-equal? (search (upper-cut "b")) (list-gap 1 (present "aa") (present "bb")))

    (check-equal? (search (lower-cut "bb")) (list-gap 1 (present "aa") (present "bb")))
    (check-equal? (search (middle-cut "bb")) (list-position 1 "bb"))
    (check-equal? (search (upper-cut "bb")) (list-gap 2 (present "bb") (present "cc")))

    (check-equal? (search (lower-cut "c")) (list-gap 2 (present "bb") (present "cc")))
    (check-equal? (search (middle-cut "c")) (list-gap 2 (present "bb") (present "cc")))
    (check-equal? (search (upper-cut "c")) (list-gap 2 (present "bb") (present "cc")))

    (check-equal? (search (lower-cut "cc")) (list-gap 2 (present "bb") (present "cc")))
    (check-equal? (search (middle-cut "cc")) (list-position 2 "cc"))
    (check-equal? (search (upper-cut "cc")) (list-gap 3 (present "cc") (present "dd")))

    (check-equal? (search (lower-cut "d")) (list-gap 3 (present "cc") (present "dd")))
    (check-equal? (search (middle-cut "d")) (list-gap 3 (present "cc") (present "dd")))
    (check-equal? (search (upper-cut "d")) (list-gap 3 (present "cc") (present "dd")))

    (check-equal? (search (lower-cut "dd")) (list-gap 3 (present "cc") (present "dd")))
    (check-equal? (search (middle-cut "dd")) (list-position 3 "dd"))
    (check-equal? (search (upper-cut "dd")) (list-gap 4 (present "dd") absent))

    (check-equal? (search (lower-cut "e")) (list-gap 4 (present "dd") absent))
    (check-equal? (search (middle-cut "e")) (list-gap 4 (present "dd") absent))
    (check-equal? (search (upper-cut "e")) (list-gap 4 (present "dd") absent))

    (check-equal? (search top-cut) (list-gap 4 (present "dd") absent))))


(define (vector-binary-search-element-less-than
         vec upper-bound [start 0] [end (vector-length vec)] #:comparator cmp)
  (list-gap-element-before
   (vector-binary-search-cut vec (lower-cut upper-bound) start end #:comparator cmp)))


(define (vector-binary-search-element-greater-than
         vec lower-bound [start 0] [end (vector-length vec)] #:comparator cmp)
  (list-gap-element-after
   (vector-binary-search-cut vec (upper-cut lower-bound) start end #:comparator cmp)))


(define (vector-binary-search-element-at-most
         vec upper-bound [start 0] [end (vector-length vec)] #:comparator cmp)
  (list-gap-element-before
   (vector-binary-search-cut vec (upper-cut upper-bound) start end #:comparator cmp)))


(define (vector-binary-search-element-at-least
         vec lower-bound [start 0] [end (vector-length vec)] #:comparator cmp)
  (list-gap-element-after
   (vector-binary-search-cut vec (lower-cut lower-bound) start end #:comparator cmp)))


(define (range-vector-binary-search range-vec value [start 0] [end (vector-length range-vec)])
  (vector-generalized-binary-search range-vec (λ (r) (range-compare-to-value r value)) start end))


(define (range-vector-binary-search-cut range-vec cut [start 0] [end (vector-length range-vec)])
  (vector-generalized-binary-search range-vec (λ (r) (range-compare-to-cut r cut)) start end))
