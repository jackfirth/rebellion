#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [sorted-set->immutable-sorted-set (-> sorted-set? immutable-sorted-set?)]))


(module+ private-for-rebellion-only
  (provide
   (contract-out
    [regular-immutable-sorted-set? predicate/c]
    [make-regular-immutable-sorted-set
     (-> (and/c vector? immutable?) comparator? immutable-sorted-set?)])))


(require racket/contract/combinator
         racket/generic
         racket/unsafe/ops
         racket/vector
         rebellion/base/comparator
         rebellion/base/option
         (submod rebellion/base/range private-for-rebellion-only)
         (submod rebellion/collection/private/persistent-sorted-set private-for-rebellion-only)
         rebellion/collection/private/reversed-sorted-set
         rebellion/collection/private/sorted-set-interface
         (submod rebellion/collection/private/sorted-set-interface private-for-rebellion-only)
         rebellion/collection/private/vector-binary-search
         rebellion/private/guarded-block)


;@----------------------------------------------------------------------------------------------------


(define (sorted-set->immutable-sorted-set set)
  (cond
    [(regular-immutable-sorted-set? set) set]
    [(regular-immutable-sorted-subset? set)
     (define start (regular-immutable-sorted-subset-start-index set))
     (define end (regular-immutable-sorted-subset-end-index))
     (define vec (make-vector (- end start)))
     (vector-copy! vec (regular-immutable-sorted-subset-sorted-vector set) start end)
     (make-regular-immutable-sorted-set
      (unsafe-vector*->immutable-vector! vec) (sorted-set-comparator set))]
    [else
     (define vec
       (unsafe-vector*->immutable-vector!
        (for/vector #:length (sorted-set-size set) ([element (in-sorted-set set)])
          element)))
     (make-regular-immutable-sorted-set vec (sorted-set-comparator set))]))


(struct abstract-regular-immutable-sorted-set abstract-immutable-sorted-set ()

  #:methods gen:immutable-sorted-set

  [(define/generic generic-sorted-set-add sorted-set-add)
   (define/generic generic-sorted-set-add-all sorted-set-add-all)
   (define/generic generic-sorted-set-remove sorted-set-remove)
   (define/generic generic-sorted-set-remove-all sorted-set-remove-all)

   (define (sorted-set-add this element)
     (generic-sorted-set-add (make-persistent-sorted-set-from-sorted this) element))

   (define (sorted-set-add-all this elements)
     (generic-sorted-set-add-all (make-persistent-sorted-set-from-sorted this) elements))

   (define (sorted-set-remove this element)
     (generic-sorted-set-remove (make-persistent-sorted-set-from-sorted this) element))

   (define (sorted-set-remove-all this elements)
     (generic-sorted-set-remove-all (make-persistent-sorted-set-from-sorted this) elements))])


(struct regular-immutable-sorted-set abstract-regular-immutable-sorted-set (sorted-vector comparator)

  #:constructor-name make-regular-immutable-sorted-set

  #:methods gen:sorted-set

  [(define (in-sorted-set this #:descending? [descending? #false])
     (define vec (regular-immutable-sorted-set-sorted-vector this))
     (if descending?
         (in-vector vec (sub1 (vector-length vec)) -1 -1)
         (in-vector vec)))

   (define (sorted-set-empty? this)
     (vector-empty? (regular-immutable-sorted-set-sorted-vector this)))

   (define (sorted-set-size this)
     (vector-length (regular-immutable-sorted-set-sorted-vector this)))

   (define (sorted-set-comparator this)
     (regular-immutable-sorted-set-comparator this))

   (define/guard (sorted-set-contains? this value)
     (define vec (regular-immutable-sorted-set-sorted-vector this))
     (define cmp (regular-immutable-sorted-set-comparator this))
     (and (contract-first-order-passes? (comparator-operand-contract cmp) value)
          (position? (vector-binary-search vec value #:comparator cmp))))

   (define (sorted-set-least-element this)
     (define vec (regular-immutable-sorted-set-sorted-vector this))
     (if (vector-empty? vec) absent (present (vector-ref vec 0))))

   (define (sorted-set-greatest-element this)
     (define vec (regular-immutable-sorted-set-sorted-vector this))
     (if (vector-empty? vec) absent (present (vector-ref vec (sub1 (vector-length vec))))))

   (define (sorted-set-element-less-than this upper-bound)
     (define vec (regular-immutable-sorted-set-sorted-vector this))
     (define cmp (regular-immutable-sorted-set-comparator this))
     (vector-binary-search-element-less-than vec upper-bound #:comparator cmp))

   (define (sorted-set-element-greater-than this lower-bound)
     (define vec (regular-immutable-sorted-set-sorted-vector this))
     (define cmp (regular-immutable-sorted-set-comparator this))
     (vector-binary-search-element-greater-than vec lower-bound #:comparator cmp))

   (define (sorted-set-element-at-most this upper-bound)
     (define vec (regular-immutable-sorted-set-sorted-vector this))
     (define cmp (regular-immutable-sorted-set-comparator this))
     (vector-binary-search-element-at-most vec upper-bound #:comparator cmp))

   (define (sorted-set-element-at-least this lower-bound)
     (define vec (regular-immutable-sorted-set-sorted-vector this))
     (define cmp (regular-immutable-sorted-set-comparator this))
     (vector-binary-search-element-at-least vec lower-bound #:comparator cmp))

   (define (sorted-subset this element-range)
     (define vec (regular-immutable-sorted-set-sorted-vector this))
     (define cmp (regular-immutable-sorted-set-comparator this))
     (define start
       (gap-index (vector-binary-search-cut vec (range-lower-cut element-range) #:comparator cmp)))
     (define end
       (gap-index (vector-binary-search-cut vec (range-upper-cut element-range) #:comparator cmp)))
     (cond
       [(equal? start end) (empty-sorted-set cmp)]
       [(and (zero? start) (equal? end (vector-length vec))) this]
       [else (constructor:regular-immutable-sorted-subset vec cmp start end)]))

   (define (sorted-set-reverse this)
     (make-reversed-immutable-sorted-set this))])


(struct regular-immutable-sorted-subset abstract-regular-immutable-sorted-set

  (sorted-vector comparator start-index end-index)

  #:constructor-name constructor:regular-immutable-sorted-subset

  #:methods gen:sorted-set

  [(define (in-sorted-set this #:descending? [descending? #false])
     (define vec (regular-immutable-sorted-subset-sorted-vector this))
     (define start (regular-immutable-sorted-subset-start-index this))
     (define end (regular-immutable-sorted-subset-end-index this))
     (if descending?
         (in-vector vec (sub1 end) (sub1 start) -1)
         (in-vector vec start end)))

   (define (sorted-set-empty? this)
     (define start (regular-immutable-sorted-subset-start-index this))
     (define end (regular-immutable-sorted-subset-end-index this))
     (equal? start end))

   (define (sorted-set-size this)
     (define start (regular-immutable-sorted-subset-start-index this))
     (define end (regular-immutable-sorted-subset-end-index this))
     (- end start))

   (define (sorted-set-comparator this)
     (regular-immutable-sorted-subset-comparator this))

   (define (sorted-set-contains? this value)
     (define vec (regular-immutable-sorted-subset-sorted-vector this))
     (define cmp (regular-immutable-sorted-subset-comparator this))
     (define start (regular-immutable-sorted-subset-start-index this))
     (define end (regular-immutable-sorted-subset-end-index this))
     (and (contract-first-order-passes? (comparator-operand-contract cmp) value)
          (position? (vector-binary-search vec value start end #:comparator cmp))))

   (define/guard (sorted-set-least-element this)
     (guard (sorted-set-empty? this) then
       absent)
     (define vec (regular-immutable-sorted-subset-sorted-vector this))
     (define start (regular-immutable-sorted-subset-start-index this))
     (present (vector-ref vec start)))

   (define (sorted-set-greatest-element this)
     (define vec (regular-immutable-sorted-subset-sorted-vector this))
     (define end (regular-immutable-sorted-subset-end-index this))
     (if (zero? end) absent (present (vector-ref vec (sub1 end)))))

   (define (sorted-set-element-less-than this upper-bound)
     (define vec (regular-immutable-sorted-subset-sorted-vector this))
     (define cmp (regular-immutable-sorted-subset-comparator this))
     (define start (regular-immutable-sorted-subset-start-index this))
     (define end (regular-immutable-sorted-subset-end-index this))
     (vector-binary-search-element-less-than vec upper-bound start end #:comparator cmp))

   (define (sorted-set-element-greater-than this lower-bound)
     (define vec (regular-immutable-sorted-subset-sorted-vector this))
     (define cmp (regular-immutable-sorted-subset-comparator this))
     (define start (regular-immutable-sorted-subset-start-index this))
     (define end (regular-immutable-sorted-subset-end-index this))
     (vector-binary-search-element-greater-than vec lower-bound start end #:comparator cmp))

   (define (sorted-set-element-at-most this upper-bound)
     (define vec (regular-immutable-sorted-subset-sorted-vector this))
     (define cmp (regular-immutable-sorted-subset-comparator this))
     (define start (regular-immutable-sorted-subset-start-index this))
     (define end (regular-immutable-sorted-subset-end-index this))
     (vector-binary-search-element-at-most vec upper-bound start end #:comparator cmp))

   (define (sorted-set-element-at-least this lower-bound)
     (define vec (regular-immutable-sorted-subset-sorted-vector this))
     (define cmp (regular-immutable-sorted-subset-comparator this))
     (define start (regular-immutable-sorted-subset-start-index this))
     (define end (regular-immutable-sorted-subset-end-index this))
     (vector-binary-search-element-at-least vec lower-bound start end #:comparator cmp))

   (define (sorted-subset this element-range)
     (define vec (regular-immutable-sorted-subset-sorted-vector this))
     (define cmp (regular-immutable-sorted-subset-comparator this))
     (define start (regular-immutable-sorted-subset-start-index this))
     (define end (regular-immutable-sorted-subset-end-index this))
     (define new-start
       (gap-index
        (vector-binary-search-cut vec (range-lower-cut element-range) start end #:comparator cmp)))
     (define new-end
       (gap-index
        (vector-binary-search-cut vec (range-upper-cut element-range) start end #:comparator cmp)))
     (if (equal? new-start new-end)
         (empty-sorted-set cmp)
         (constructor:regular-immutable-sorted-subset vec cmp new-start new-end)))

   (define (sorted-set-reverse this)
     (make-reversed-immutable-sorted-set this))])
