#lang racket/base


(require racket/contract/base)


(provide
 (struct-out upper-cut)
 (struct-out middle-cut)
 (struct-out lower-cut)
 (contract-out
  [cut? predicate/c]
  [top-cut cut?]
  [bottom-cut cut?]
  [intermediate-cut? predicate/c]
  [intermediate-cut-value (-> intermediate-cut? any/c)]
  [cut<=> (-> comparator? (comparator/c cut?))]
  [cut-flip-side (-> cut? cut?)]))


(require racket/match
         rebellion/base/comparator
         guard
         rebellion/private/static-name)


;@----------------------------------------------------------------------------------------------------


(struct cut () #:transparent)
(struct intermediate-cut cut () #:transparent)
(struct upper-cut intermediate-cut (value) #:transparent)
(struct middle-cut intermediate-cut (value) #:transparent)
(struct lower-cut intermediate-cut (value) #:transparent)


(struct top-cut cut ()
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name constructor:top-cut)


(struct bottom-cut cut ()
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name constructor:bottom-cut)


(define top-cut (constructor:top-cut))
(define bottom-cut (constructor:bottom-cut))


(define (intermediate-cut-value cut)
  (match cut
    [(upper-cut value) value]
    [(middle-cut value) value]
    [(lower-cut value) value]))


(define (cut<=> base-comparator)
  ;; Using a cache ensures that a == b implies (cut<=> a) == (cut<=> b)
  (hash-ref!
   cut-comparator-cache
   base-comparator
   (λ () (make-comparator (cut-compare base-comparator) #:name (name cut<=>)))))


(define cut-comparator-cache (make-ephemeron-hash))


(define/guard ((cut-compare base-comparator) left right)
  (cond
    [(and (equal? left bottom-cut) (equal? right bottom-cut)) equivalent]
    [(equal? left bottom-cut) lesser]
    [(equal? right bottom-cut) greater]
    [(and (equal? left top-cut) (equal? right top-cut)) equivalent]
    [(equal? left top-cut) greater]
    [(equal? right top-cut) lesser]
    [else
     (define result
       (compare
        base-comparator
        (intermediate-cut-value left)
        (intermediate-cut-value right)))
     (cond
       [(or (equal? result lesser) (equal? result greater)) result]
       [(and (lower-cut? left) (lower-cut? right)) equivalent]
       [(lower-cut? left) lesser]
       [(lower-cut? right) greater]
       [(and (middle-cut? left) (middle-cut? right)) equivalent]
       [(middle-cut? left) lesser]
       [(middle-cut? right) greater]
       [else equivalent])]))


(define (cut-flip-side cut)
  (match cut
    [(upper-cut value) (lower-cut value)]
    [(lower-cut value) (upper-cut value)]
    [_ cut]))
