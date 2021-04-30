#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [sorted-set? predicate/c]
  [immutable-sorted-set? predicate/c]
  [mutable-sorted-set? predicate/c]
  [make-mutable-sorted-set (-> comparator? mutable-sorted-set?)]
  [sorted-set (-> #:comparator comparator? any/c ... immutable-sorted-set?)]
  [empty-sorted-set (-> comparator? immutable-sorted-set?)]
  [sequence->sorted-set (-> (sequence/c any/c) #:comparator comparator? immutable-sorted-set?)]
  [in-sorted-set (-> sorted-set? (sequence/c any/c))]
  [into-sorted-set (-> comparator? (reducer/c any/c immutable-sorted-set?))]
  [sorted-set-size (-> sorted-set? natural?)]
  [sorted-set-comparator (-> sorted-set? comparator?)]
  [sorted-set-contains? (-> sorted-set? any/c boolean?)]
  [sorted-set-contains-all? (-> sorted-set? (sequence/c any/c) boolean?)]
  [sorted-set-contains-any? (-> sorted-set? (sequence/c any/c) boolean?)]
  [sorted-set-contains-none? (-> sorted-set? (sequence/c any/c) boolean?)]
  [sorted-set-least-element (-> sorted-set? option?)]
  [sorted-set-greatest-element (-> sorted-set? option?)]
  [sorted-set-element-less-than (-> sorted-set? any/c option?)]
  [sorted-set-element-at-most (-> sorted-set? any/c option?)]
  [sorted-set-element-greater-than (-> sorted-set? any/c option?)]
  [sorted-set-element-at-least (-> sorted-set? any/c option?)]
  [sorted-set-add (-> immutable-sorted-set? any/c immutable-sorted-set?)]
  [sorted-set-add! (-> mutable-sorted-set? any/c void?)]
  [sorted-set-add-all (-> immutable-sorted-set? (sequence/c any/c) immutable-sorted-set?)]
  [sorted-set-add-all! (-> mutable-sorted-set? (sequence/c any/c) void?)]
  [sorted-set-remove (-> immutable-sorted-set? any/c immutable-sorted-set?)]
  [sorted-set-remove! (-> mutable-sorted-set? any/c void?)]
  [sorted-set-remove-all (-> immutable-sorted-set? (sequence/c any/c) immutable-sorted-set?)]
  [sorted-set-remove-all! (-> mutable-sorted-set? (sequence/c any/c) void?)]
  [sorted-set-clear! (-> mutable-sorted-set? void?)]
  [sorted-subset (-> sorted-set? range? sorted-set?)]
  [sorted-set-reverse (-> sorted-set? sorted-set?)]
  [sorted-set-builder? predicate/c]
  [sorted-set-builder-add (-> sorted-set-builder? any/c any/c ... sorted-set-builder?)]
  [sorted-set-builder-add-all (-> sorted-set-builder? (sequence/c any/c) sorted-set-builder?)]
  [make-sorted-set-builder (-> comparator? sorted-set-builder?)]
  [build-sorted-set (-> sorted-set-builder? immutable-sorted-set?)]))


(require racket/math
         racket/sequence
         (only-in racket/vector vector-sort!)
         rebellion/base/comparator
         rebellion/base/option
         rebellion/base/range
         rebellion/collection/immutable-vector
         rebellion/collection/private/mutable-red-black-tree
         rebellion/collection/private/persistent-red-black-tree
         rebellion/collection/private/vector-binary-search
         rebellion/collection/vector
         rebellion/collection/vector/builder
         rebellion/private/guarded-block
         rebellion/private/static-name
         rebellion/streaming/reducer
         rebellion/streaming/transducer)


;@----------------------------------------------------------------------------------------------------
;; Sorted sets come in two flavors: immutable and mutable. They share a common supertype.


(struct sorted-set
  ()
  #:name struct-transformer:sorted-set
  #:constructor-name constructor:sorted-set)


(define (sorted-set-size set)
  (if (immutable-sorted-set? set)
      (immutable-sorted-set-size set)
      (mutable-sorted-set-size set)))


(define (sorted-set-comparator set)
  (if (immutable-sorted-set? set)
      (immutable-sorted-set-comparator set)
      (mutable-sorted-set-comparator set)))


(define (sorted-set-contains? set element)
  (if (immutable-sorted-set? set)
      (immutable-sorted-set-contains? set element)
      (mutable-sorted-set-contains? set element)))


(define (sorted-set-contains-all? set elements)
  (transduce elements #:into (into-all-match? (λ (e) (sorted-set-contains? set e)))))


(define (sorted-set-contains-any? set elements)
  (transduce elements #:into (into-any-match? (λ (e) (sorted-set-contains? set e)))))


(define (sorted-set-contains-none? set elements)
  (transduce elements #:into (into-none-match? (λ (e) (sorted-set-contains? set e)))))


(define (sorted-set-least-element set)
  (transduce set #:into into-first))


(define (sorted-set-greatest-element set)
  (sorted-set-least-element (sorted-set-reverse set)))


(define (sorted-set-element-less-than set element)
  (define range (less-than-range element #:comparator (sorted-set-comparator set)))
  (sorted-set-greatest-element (sorted-subset set range)))


(define (sorted-set-element-at-most set element)
  (define range (at-most-range element #:comparator (sorted-set-comparator set)))
  (sorted-set-greatest-element (sorted-subset set range)))


(define (sorted-set-element-greater-than set element)
  (define range (greater-than-range element #:comparator (sorted-set-comparator set)))
  (sorted-set-least-element (sorted-subset set range)))


(define (sorted-set-element-at-least set element)
  (define range (at-least-range element #:comparator (sorted-set-comparator set)))
  (sorted-set-least-element (sorted-subset set range)))


(define (sorted-subset set range)
  (if (immutable-sorted-set? set)
      (immutable-sorted-subset set range)
      (mutable-sorted-subset set range)))


(define (sorted-set-reverse set)
  (if (immutable-sorted-set? set)
      (immutable-sorted-set-reverse set)
      (mutable-sorted-set-reverse set)))


(define (in-sorted-set set)
  (if (immutable-sorted-set? set)
      (in-immutable-sorted-set set)
      (in-mutable-sorted-set set)))


;@----------------------------------------------------------------------------------------------------
;; Mutable sorted sets


(struct mutable-sorted-set struct-transformer:sorted-set
  (backing-tree)
  #:guard (struct-guard/c mutable-red-black-tree?)
  #:omit-define-syntaxes
  #:constructor-name constructor:mutable-sorted-set)


(define (make-mutable-sorted-set comparator)
  (constructor:mutable-sorted-set (make-mutable-red-black-tree comparator)))


(define (mutable-sorted-set-size set)
  (mutable-red-black-tree-size (mutable-sorted-set-backing-tree set)))


(define (mutable-sorted-set-comparator set)
  (mutable-red-black-tree-comparator (mutable-sorted-set-backing-tree set)))


(define (mutable-sorted-set-contains? set element)
  (mutable-red-black-tree-contains? (mutable-sorted-set-backing-tree set) element))


(define/guard (sorted-set-add! set element)
  (unless (sorted-set-contains? set element)
    (mutable-red-black-tree-insert! (mutable-sorted-set-backing-tree set) element)))


(define (sorted-set-add-all! set elements)
  (for ([element elements])
    (sorted-set-add! set element)))


(define (sorted-set-remove! set element)
  ;; TODO
  (void))


(define (sorted-set-remove-all! set elements)
  (for ([element elements])
    (sorted-set-remove! set element)))


(define (sorted-set-clear! set)
  (mutable-red-black-tree-clear! (mutable-sorted-set-backing-tree set)))


(define (mutable-sorted-subset set range)
  ;; TODO
  set)


(define (mutable-sorted-set-reverse set)
  ;; TODO
  set)


(define (in-mutable-sorted-set set)
  (in-mutable-red-black-tree (mutable-sorted-set-backing-tree set)))


;@----------------------------------------------------------------------------------------------------
;; Immutable sorted sets


(struct immutable-sorted-set struct-transformer:sorted-set
  
  ;; Immutable sorted sets are implemented with *either* a flat, sorted vector, *or* an immutable
  ;; persistent red-black tree. When created with a builder, the elements are stord in a vector since
  ;; future modifications are unlikely. When sorted-set-add is called the tree is used, and if the
  ;; elements are stored in a vector, a tree is derived and cached.
  (comparator backing-vector [backing-tree #:mutable])
  
  #:guard
  (struct-guard/c
   comparator? (or/c immutable-vector? #false) (or/c persistent-red-black-tree? #false))
  
  #:omit-define-syntaxes
  #:constructor-name constructor:immutable-sorted-set
  
  #:methods gen:equal+hash
  
  [(define/guard (equal-proc this other recur)
     (guard (eq? this other) then
       #true)
     (guard (equal? (immutable-sorted-set-size this) (immutable-sorted-set-size other)) else
       #false)
     (for/and ([x (in-immutable-sorted-set this)]
               [y (in-immutable-sorted-set other)])
       (recur x y)))
   
   (define (hash-proc this recur)
     (+ (recur (immutable-sorted-set-comparator this))
        (for/sum ([x (in-immutable-sorted-set this)])
          (recur x))))
   
   (define hash2-proc hash-proc)])


(define (sorted-set #:comparator comparator . elements)
  (sequence->sorted-set elements #:comparator comparator))


(define (empty-sorted-set comparator)
  (constructor:immutable-sorted-set comparator #() (empty-persistent-red-black-tree comparator)))


(define (sequence->sorted-set elements #:comparator comparator)
  (transduce elements #:into (into-sorted-set comparator)))


(define (into-sorted-set comparator)
  (make-effectful-fold-reducer
   sorted-set-builder-add
   (λ () (make-sorted-set-builder comparator))
   build-sorted-set
   #:name (name into-sorted-set)))


(define (immutable-sorted-set-size set)
  (define vector (immutable-sorted-set-backing-vector set))
  (if vector
      (vector-length vector)
      (persistent-red-black-tree-size (immutable-sorted-set-backing-tree set))))


(define (immutable-sorted-set-contains? set element)
  (define element<=> (immutable-sorted-set-comparator set))
  (define vector (immutable-sorted-set-backing-vector set))
  (if vector
      (exact-match? (vector-binary-search vector (λ (x) (compare element<=> x element))))
      (persistent-red-black-tree-contains? (immutable-sorted-set-backing-tree set) element)))


(define/guard (sorted-set-add set element)
  (guard (immutable-sorted-set-contains? set element) then
    set)
  (immutable-sorted-set-materialize-tree! set)
  (define modified-tree
    (persistent-red-black-tree-insert (immutable-sorted-set-backing-tree set) element))
  (constructor:immutable-sorted-set (immutable-sorted-set-comparator set) #false modified-tree))


(define (sorted-set-add-all set elements)
  (transduce elements #:into (make-fold-reducer sorted-set-add set)))


(define/guard (sorted-set-remove set element)
  (guard (immutable-sorted-set-contains? set element) else
    set)
  (immutable-sorted-set-materialize-tree! set)
  (define modified-tree
    (persistent-red-black-tree-remove (immutable-sorted-set-backing-tree set) element))
  (constructor:immutable-sorted-set (immutable-sorted-set-comparator set) #false modified-tree))


(define (sorted-set-remove-all set elements)
  (transduce elements #:into (make-fold-reducer sorted-set-remove set)))


(define (immutable-sorted-subset set range)
  ;; TODO
  set)


(define (immutable-sorted-set-reverse set)
  ;; TODO
  set)


(define (in-immutable-sorted-set set)
  (define vector (immutable-sorted-set-backing-vector set))
  (if vector
      (in-vector vector)
      (in-persistent-red-black-tree (immutable-sorted-set-backing-tree set))))


(define (immutable-sorted-set-materialize-tree! set)
  ;; TODO: this isn't thread-safe. Maybe make a monotonic-box library, or use promises?
  (unless (immutable-sorted-set-backing-tree set)
    (define element<=> (immutable-sorted-set-comparator set))
    (define vector (immutable-sorted-set-backing-vector set))
    (define tree (sorted-unique-sequence->persistent-red-black-tree vector element<=>))
    (set-immutable-sorted-set-backing-tree! set tree)))


;@----------------------------------------------------------------------------------------------------
;; Sorted set builders


(struct sorted-set-builder
  (comparator vector-builder)
  #:guard (struct-guard/c comparator? vector-builder?)
  #:omit-define-syntaxes
  #:constructor-name constructor:sorted-set-builder)


(define (make-sorted-set-builder comparator)
  (constructor:sorted-set-builder comparator (make-vector-builder)))


(define (sorted-set-builder-add builder . elements)
  (sorted-set-builder-add-all builder elements))


(define (sorted-set-builder-add-all builder elements)
  (define modified (vector-builder-add-all (sorted-set-builder-vector-builder builder) elements))
  (constructor:sorted-set-builder (sorted-set-builder-comparator builder) modified))


(define/guard (build-sorted-set builder)
  (define element<=> (sorted-set-builder-comparator builder))
  (define mutable-elements (build-mutable-vector (sorted-set-builder-vector-builder builder)))
  (guard (zero? (vector-length mutable-elements)) then
    (empty-sorted-set element<=>))
  
  (define (< x y)
    (equal? (compare element<=> x y) lesser))
  
  (vector-sort! mutable-elements <)
  (define deduplicated
    (transduce mutable-elements
               (deduplicating-consecutive)
               #:into (into-vector #:size (vector-length mutable-elements))))
  (constructor:immutable-sorted-set element<=> deduplicated #false))
