#lang racket/base


(module+ test

  (provide test-sorted-set)
  
  (require racket/match
           racket/sequence
           rackunit
           rebellion/base/comparator
           rebellion/base/option
           rebellion/base/range
           rebellion/collection/sorted-set
           rebellion/concurrency/lock
           (only-in racket/list shuffle)))


;@----------------------------------------------------------------------------------------------------


(module+ test

  (define (test-sorted-set set)

    (with-check-info (['set set])

      (test-case "sorted sets should be sequences"
        (check-pred sequence? set))
    
      (test-case "sorted-set-size"
        (define declared-size (sorted-set-size set))
        (define max-size-estimate (+ declared-size 10))
        
        ;; In case of buggy prop:sequence implementations that iterate forever, we only check if the
        ;; length of the sequence is greater than expected-size rather than trying to determine the
        ;; exact length of the sequence.
        (define size-estimate
          (for/sum ([v (in-sorted-set set)]
                    [i (in-range 0 max-size-estimate)])
            1))
        (define true-size
          (if (equal? size-estimate max-size-estimate)
              'too-many
              size-estimate))
        (check-equal? true-size declared-size))

      (test-case "sorted-set-empty?"
        (cond
          [(zero? (sorted-set-size set))
           (check-true (sorted-set-empty? set))]
          [else
           (check-false (sorted-set-empty? set))]))

      (define size (sorted-set-size set))
      (define cmp (sorted-set-comparator set))
    
      (test-case "in-sorted-set"
        (for/fold ([previous-opt absent] #:result (void))
                  ([next (in-sorted-set set)])
          (when (present? previous-opt)
            (match-define (present previous) previous-opt)
            (check-equal? (compare cmp previous next) lesser))
          (present next)))

      (test-case "in-sorted-set #:descending"
        (for/fold ([previous-opt absent] #:result (void))
                  ([next (in-sorted-set set #:descending? #true)])
          (when (present? previous-opt)
            (match-define (present previous) previous-opt)
            (check-equal? (compare cmp previous next) greater))
          (present next)))

      (test-case "sorted-set-contains?"
        (check-false (sorted-set-contains? set (gensym)))
        (for ([element (in-sorted-set set)])
          (with-check-info (['element element])
            (check-true (sorted-set-contains? set element)))))

      (test-case "sorted-set-contains-all?"
        (check-true (sorted-set-contains-all? set (list)))
        (for ([element (in-sorted-set set)])
          (check-true (sorted-set-contains-all? set (list element)))
          (check-false (sorted-set-contains-all? set (list (gensym))))
          (check-true (sorted-set-contains-all? set (list element element element)))
          (check-false (sorted-set-contains-all? set (list element (gensym) (gensym))))))

      (test-case "sorted-set-contains-any?"
        (check-false (sorted-set-contains-any? set (list)))
        (for ([element (in-sorted-set set)])
          (check-true (sorted-set-contains-any? set (list element)))
          (check-false (sorted-set-contains-any? set (list (gensym))))
          (check-true (sorted-set-contains-any? set (list element element element)))
          (check-true (sorted-set-contains-any? set (list element (gensym))))))

      (test-case "sorted-set-contains-none?"
        (check-true (sorted-set-contains-none? set (list)))
        (for ([element (in-sorted-set set)])
          (check-false (sorted-set-contains-none? set (list element)))
          (check-true (sorted-set-contains-none? set (list (gensym))))
          (check-false (sorted-set-contains-none? set (list element element element)))
          (check-false (sorted-set-contains-none? set (list element (gensym))))))

      (test-case "sorted-set-least-element"
        (cond
          [(sorted-set-empty? set)
           (check-equal? (sorted-set-least-element set) absent)]
          [else
           (check-equal? (sorted-set-least-element set) (present (sequence-ref set 0)))]))

      (test-case "sorted-set-greatest-element"
        (cond
          [(sorted-set-empty? set)
           (check-equal? (sorted-set-greatest-element set) absent)]
          [else
           (define last (sequence-ref set (sub1 size)))
           (check-equal? (sorted-set-greatest-element set) (present last))]))

      (test-case "sorted-set-element-less-than"
        (for/fold ([previous absent] #:result (void))
                  ([next (in-sorted-set set)])
          (with-check-info (['upper-bound next])
            (check-equal? (sorted-set-element-less-than set next) previous)
            (present next))))

      (test-case "sorted-set-element-greater-than"
        (for/fold ([previous absent] #:result (void))
                  ([next (in-sorted-set set #:descending? #true)])
          (with-check-info (['lower-bound next])
            (check-equal? (sorted-set-element-greater-than set next) previous)
            (present next))))

      (test-case "sorted-set-element-at-most"
        (for ([element (in-sorted-set set)])
          (with-check-info (['upper-bound element])
            (check-equal? (sorted-set-element-at-most set element) (present element)))))

      (test-case "sorted-set-element-at-least"
        (for ([element (in-sorted-set set)])
          (with-check-info (['lower-bound element])
            (check-equal? (sorted-set-element-at-least set element) (present element)))))

      (test-case "sorted-set-size on exclusively bounded-above subsets"
        (for ([element (in-sorted-set set)]
              [i (in-naturals)])
          (define subset (sorted-subset set (less-than-range element #:comparator cmp)))
          (check-equal? (sorted-set-size subset) i)))

      (test-case "sorted-set-size on exclusively bounded-below subsets"
        (for ([element (in-sorted-set set #:descending? #true)]
              [i (in-naturals)])
          (define subset (sorted-subset set (greater-than-range element #:comparator cmp)))
          (check-equal? (sorted-set-size subset) i)))

      (test-case "sorted-set-size on inclusively bounded-above subsets"
        (for ([element (in-sorted-set set)]
              [i (in-naturals 1)])
          (define subset (sorted-subset set (at-most-range element #:comparator cmp)))
          (check-equal? (sorted-set-size subset) i)))

      (test-case "sorted-set-size on inclusively bounded-below subsets"
        (for ([element (in-sorted-set set #:descending? #true)]
              [i (in-naturals 1)])
          (define subset (sorted-subset set (at-least-range element #:comparator cmp)))
          (check-equal? (sorted-set-size subset) i)))))


  (define (test-immutable-sorted-set set #:try-inserting insertable-elements)

    (with-check-info (['set set])

      (test-sorted-set set)

      (test-case "sorted-set-add"

        (test-case "sorted-set-add already contained elements"
          (for ([e set])
            (check-equal? (sorted-set-add set e) set)))

        (test-case "sorted-set-add new elements"
          (for ([e insertable-elements])
            (with-check-info (['inserted-element e])
              (check-false (sorted-set-contains? set e))
              (define modified (sorted-set-add set e))
              (check-equal? (sorted-set-size modified) (add1 (sorted-set-size set)))
              (check-true (sorted-set-contains? modified e))
              (test-sorted-set modified)))))

      (test-case "sorted-set-remove"

        (test-case "sorted-set-remove elements not contained by set"
          (for ([e insertable-elements])
            (with-check-info (['element e])
              (check-false (sorted-set-contains? set e))
              (check-equal? (sorted-set-remove set e) set))))

        (test-case "sorted-set-remove contained elements"
          (for ([e set])
            (with-check-info (['removed-element e])
              (define modified (sorted-set-remove set e))
              (check-equal? (sorted-set-size modified) (sub1 (sorted-set-size set)))
              (check-false (sorted-set-contains? modified e))
              (test-sorted-set modified)))))))


  (test-case "sorted set equality"
    (check-equal? (sorted-set #:comparator natural<=>) (sorted-set #:comparator natural<=>))
    (check-not-equal? (sorted-set #:comparator natural<=>) (sorted-set #:comparator string<=>))
    (check-not-equal? (sorted-set 1 2 3 #:comparator natural<=>) (sorted-set #:comparator natural<=>))
    (check-equal?
     (sorted-set 1 2 3 #:comparator natural<=>) (sorted-set 1 2 3 #:comparator natural<=>))
    (check-equal?
     (sorted-set 1 2 3 #:comparator natural<=>) (sorted-set 3 2 1 #:comparator natural<=>))
    (check-not-equal? (sorted-set 1 #:comparator natural<=>) (sorted-set 2 #:comparator natural<=>)))

  (test-case "immutable sorted set"
    
    (test-case "empty immutable sorted set"
      (test-immutable-sorted-set (sorted-set #:comparator natural<=>) #:try-inserting (list 1 2 3)))

    (test-case "singleton immutable sorted set"
      (test-immutable-sorted-set (sorted-set 1 #:comparator natural<=>) #:try-inserting (list 2 3 4)))

    (define set (sorted-set 1 2 3 4 5 6 7 8 9 10 #:comparator real<=>))
    (test-immutable-sorted-set set #:try-inserting (list 0 20 4.5))

    (test-case "immutable sorted subset"
      (test-immutable-sorted-set
       (sorted-subset set (closed-range 3 8)) #:try-inserting (list 0 1 20 10 4.5)))

    (test-case "reversed immutable sorted set"
      (test-immutable-sorted-set (sorted-set-reverse set) #:try-inserting (list 0 20 4.5)))

    (test-case "reversed immutable sorted subset"
      (define range (closed-range 8 3 #:comparator (comparator-reverse real<=>)))
      (test-immutable-sorted-set
       (sorted-subset (sorted-set-reverse set) range) #:try-inserting (list 0 1 20 10 4.5)))

    (test-case "unmodifiable view"
      (define unmodifiable (unmodifiable-sorted-set set))
      (check-equal? unmodifiable set)
      (check-true (immutable-sorted-set? unmodifiable))
      (test-immutable-sorted-set unmodifiable #:try-inserting (list 0 20 4.5))))

  (test-case "mutable sorted set"

    (define set (make-mutable-sorted-set (list 1 2 3 4 5 6 7 8 9 10) #:comparator real<=>))
    (test-sorted-set set)

    (test-case "mutable sorted subset"
      (test-sorted-set (sorted-subset set (closed-range 3 8))))

    (test-case "reversed mutable sorted set"
      (test-sorted-set (sorted-set-reverse set)))

    (test-case "reversed mutable sorted subset"
      (define range (closed-range 8 3 #:comparator (comparator-reverse real<=>)))
      (test-sorted-set (sorted-subset (sorted-set-reverse set) range)))

    (test-case "unmodifiable view"
      (define unmodifiable (unmodifiable-sorted-set set))
      (check-not-equal? unmodifiable set)
      (check-false (immutable-sorted-set? unmodifiable))
      (check-false (mutable-sorted-set? unmodifiable))
      (test-sorted-set unmodifiable))

    (test-case "synchronized view"
      (define synchronized (synchronized-sorted-set set))
      (check-true (mutable-sorted-set? synchronized))
      (test-sorted-set synchronized)

      (define lock (synchronized-sorted-set-lock synchronized))

      (test-case "externally locked by reader"
        (lock! (read-write-lock-read-lock lock) (λ () (test-sorted-set synchronized))))

      (test-case "externally locked by writer"
        (lock! (read-write-lock-write-lock lock) (λ () (test-sorted-set synchronized))))

      (test-case "concurrency test"
        (define numbers (synchronized-sorted-set (make-mutable-sorted-set #:comparator natural<=>)))
        (for ([x (in-list (shuffle (sequence->list (in-range 0 1000))))])
          (thread
           (λ ()
             (for ([_ (in-range 0 1000)])
               (sorted-set-add! synchronized x)
               (sorted-set-remove! synchronized x)))))
        (sync (system-idle-evt))
        (check-true (sorted-set-empty? numbers))
        (test-sorted-set numbers)))))
