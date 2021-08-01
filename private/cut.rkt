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
  [cut<=> (-> comparator? (comparator/c cut?))]))


(require racket/match
         rebellion/base/comparator
         rebellion/private/guarded-block
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
  (define/guard (cmp left right)
    (guard (and (equal? left bottom-cut) (equal? right bottom-cut)) then
      equivalent)
    (guard (equal? left bottom-cut) then
      lesser)
    (guard (equal? right bottom-cut) then
      greater)
    (guard (and (equal? left top-cut) (equal? right top-cut)) then
      equivalent)
    (guard (equal? left top-cut) then
      greater)
    (guard (equal? right top-cut) then
      lesser)
    (define result
      (compare
       base-comparator
       (intermediate-cut-value left)
       (intermediate-cut-value right)))
    (guard (or (equal? result lesser) (equal? result greater)) then
      result)
    (guard (and (lower-cut? left) (lower-cut? right)) then
      equivalent)
    (guard (lower-cut? left) then
      lesser)
    (guard (lower-cut? right) then
      greater)
    (guard (and (middle-cut? left) (middle-cut? right)) then
      equivalent)
    (guard (middle-cut? left) then
      lesser)
    (guard (middle-cut? right) then
      greater)
    equivalent)
  (make-comparator cmp #:name (name cut<=>)))
