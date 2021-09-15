#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [unmodifiable-sorted-set (-> sorted-set? (and/c sorted-set? (not/c mutable-sorted-set?)))]))


(require racket/generic
         rebellion/collection/private/reversed-sorted-set
         rebellion/collection/private/sorted-set-interface
         (submod rebellion/collection/private/sorted-set-interface private-for-rebellion-only))


;@----------------------------------------------------------------------------------------------------


(define (unmodifiable-sorted-set set)
  (if (mutable-sorted-set? set)
      (constructor:unmodifiable-sorted-set set)
      set))


(struct unmodifiable-sorted-set abstract-sorted-set (delegate-set)

  #:omit-define-syntaxes

  #:constructor-name constructor:unmodifiable-sorted-set

  #:methods gen:sorted-set

  [(define (get-delegate this)
     (unmodifiable-sorted-set-delegate-set this))

   (define (construct delegate-set)
     (constructor:unmodifiable-sorted-set delegate-set))

   (define/generic generic-in-sorted-set in-sorted-set)
   (define/generic generic-sorted-set-empty? sorted-set-empty?)
   (define/generic generic-sorted-set-size sorted-set-size)
   (define/generic generic-sorted-set-comparator sorted-set-comparator)
   (define/generic generic-sorted-set-contains? sorted-set-contains?)
   (define/generic generic-sorted-set-contains-any? sorted-set-contains-any?)
   (define/generic generic-sorted-set-contains-all? sorted-set-contains-all?)
   (define/generic generic-sorted-set-contains-none? sorted-set-contains-none?)
   (define/generic generic-sorted-set-least-element sorted-set-least-element)
   (define/generic generic-sorted-set-greatest-element sorted-set-greatest-element)
   (define/generic generic-sorted-set-element-less-than sorted-set-element-less-than)
   (define/generic generic-sorted-set-element-greater-than sorted-set-element-greater-than)
   (define/generic generic-sorted-set-element-at-least sorted-set-element-at-least)
   (define/generic generic-sorted-set-element-at-most sorted-set-element-at-most)
   (define/generic generic-sorted-subset sorted-subset)
   (define/generic generic-sorted-set-reverse sorted-set-reverse)

   (define (in-sorted-set this #:descending? [descending? #false])
     (generic-in-sorted-set (get-delegate this) #:descending? descending?))

   (define (sorted-set-empty? this)
     (generic-sorted-set-empty? (get-delegate this)))

   (define (sorted-set-size this)
     (generic-sorted-set-size (get-delegate this)))

   (define (sorted-set-comparator this)
     (generic-sorted-set-comparator (get-delegate this)))

   (define (sorted-set-contains? this value)
     (generic-sorted-set-contains? (get-delegate this) value))

   (define (sorted-set-contains-any? this values)
     (generic-sorted-set-contains-any? (get-delegate this) values))

   (define (sorted-set-contains-all? this values)
     (generic-sorted-set-contains-all? (get-delegate this) values))

   (define (sorted-set-contains-none? this values)
     (generic-sorted-set-contains-none? (get-delegate this) values))

   (define (sorted-set-least-element this)
     (generic-sorted-set-least-element (get-delegate this)))

   (define (sorted-set-greatest-element this)
     (generic-sorted-set-greatest-element (get-delegate this)))

   (define (sorted-set-element-less-than this upper-bound)
     (generic-sorted-set-element-less-than (get-delegate this) upper-bound))

   (define (sorted-set-element-greater-than this lower-bound)
     (generic-sorted-set-element-greater-than (get-delegate this) lower-bound))

   (define (sorted-set-element-at-most this upper-bound)
     (generic-sorted-set-element-at-most (get-delegate this) upper-bound))
   
   (define (sorted-set-element-at-least this lower-bound)
     (generic-sorted-set-element-at-least (get-delegate this) lower-bound))

   (define (sorted-subset this element-range)
     (construct (generic-sorted-subset (get-delegate this) element-range)))

   (define (sorted-set-reverse this)
     (construct (generic-sorted-set-reverse (get-delegate this))))])
