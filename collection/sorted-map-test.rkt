#lang racket/base


(module+ test
  (require racket/match
           racket/sequence
           rackunit
           rebellion/base/comparator
           rebellion/base/option
           rebellion/base/range
           rebellion/collection/entry
           (submod rebellion/collection/private/regular-immutable-sorted-map
                   private-for-rebellion-only)
           rebellion/collection/sorted-map
           (submod rebellion/collection/sorted-set-test test)))


;@----------------------------------------------------------------------------------------------------


(module+ test

  (define (test-sorted-map map)

    (with-check-info (['map map])

      (test-case "sorted maps should be sequences"
        (check-pred sequence? map))
    
      (test-case "sorted-map-size"
        (define declared-size (sorted-map-size map))
        (define max-size-estimate (+ declared-size 10))
        
        ;; In case of buggy prop:sequence implementations that iterate forever, we only check if the
        ;; length of the sequence is greater than expected-size rather than trying to determine the
        ;; exact length of the sequence.
        (define size-estimate
          (for/sum ([_ (in-sorted-map map)]
                    [i (in-range 0 max-size-estimate)])
            1))
        (define true-size
          (if (equal? size-estimate max-size-estimate)
              'too-many
              size-estimate))
        (check-equal? true-size declared-size))

      (test-case "sorted-map-empty?"
        (cond
          [(zero? (sorted-map-size map))
           (check-true (sorted-map-empty? map))]
          [else
           (check-false (sorted-map-empty? map))]))

      (define key<=> (sorted-map-key-comparator map))
    
      (test-case "in-sorted-map"
        (for/fold ([previous-key-opt absent] #:result (void))
                  ([next (in-sorted-map map)])
          (define next-key (entry-key next))
          (when (present? previous-key-opt)
            (match-define (present previous-key) previous-key-opt)
            (check-equal? (compare key<=> previous-key next-key) lesser))
          (present next-key)))

      (test-case "in-sorted-map #:descending"
        (for/fold ([previous-key-opt absent] #:result (void))
                  ([next (in-sorted-map map #:descending? #true)])
          (define next-key (entry-key next))
          (when (present? previous-key-opt)
            (match-define (present previous-key) previous-key-opt)
            (check-equal? (compare key<=> previous-key next-key) greater))
          (present next-key)))

      (test-case "in-sorted-map-keys"
        (for ([entry (in-sorted-map map)]
              [key (in-sorted-map-keys map)])
          (check-equal? key (entry-key entry))))

      (test-case "in-sorted-map-keys #:descending"
        (for ([entry (in-sorted-map map #:descending? #true)]
              [key (in-sorted-map-keys map #:descending? #true)])
          (check-equal? key (entry-key entry))))

      (test-case "in-sorted-map-values"
        (for ([entry (in-sorted-map map)]
              [value (in-sorted-map-values map)])
          (check-equal? value (entry-value entry))))

      (test-case "in-sorted-map-values #:descending"
        (for ([entry (in-sorted-map map #:descending? #true)]
              [value (in-sorted-map-values map #:descending? #true)])
          (check-equal? value (entry-value entry))))

      (test-case "key set view"
        (test-sorted-set (sorted-map-keys map)))

      (test-case "entry set view"
        (test-sorted-set (sorted-map-entries map)))

      (test-case "sorted-map-contains-key?"
        (check-false (sorted-map-contains-key? map (gensym)))
        (for ([key (in-sorted-map-keys map)])
          (with-check-info (['key key])
            (check-true (sorted-map-contains-key? map key)))))

      (test-case "sorted-map-contains-value?"
        (check-false (sorted-map-contains-value? map (gensym)))
        (for ([value (in-sorted-map-values map)])
          (with-check-info (['value value])
            (check-true (sorted-map-contains-value? map value)))))

      (test-case "sorted-map-contains-entry?"
        (for ([e (in-sorted-map map)])
          (match-define (entry key value) e)
          (with-check-info (['entry e])
            (check-true (sorted-map-contains-entry? map e)))
          (define fresh-entry (entry key (gensym)))
          (with-check-info (['fresh-entry fresh-entry])
            (check-false (sorted-map-contains-entry? map fresh-entry)))))

      (test-case "sorted-map-get"
        (for ([e (in-sorted-map map)])
          (match-define (entry key value) e)
          (with-check-info (['key key])
            (check-equal? (sorted-map-get map key) value)))
        (check-exn exn:fail:contract? (λ () (sorted-map-get map (gensym))))
        (check-exn #rx"sorted-map-get:" (λ () (sorted-map-get map (gensym))))
        (check-equal? (sorted-map-get map (gensym) 'foo) 'foo)
        (check-equal? (sorted-map-get map (gensym) (λ () 'foo)) 'foo))

      (test-case "sorted-map-get-option"
        (for ([e (in-sorted-map map)])
          (match-define (entry key value) e)
          (with-check-info (['key key])
            (check-equal? (sorted-map-get-option map key) (present value))))
        (check-equal? (sorted-map-get-option map (gensym)) absent))

      (test-case "sorted-map-get-entry"
        (for ([e (in-sorted-map map)])
          (match-define (entry key value) e)
          (with-check-info (['key key])
            (check-equal? (sorted-map-get-entry map key) e)))
        (check-exn exn:fail:contract? (λ () (sorted-map-get-entry map (gensym))))
        (check-exn #rx"sorted-map-get-entry:" (λ () (sorted-map-get-entry map (gensym))))
        (define fresh-key (gensym))
        (check-equal? (sorted-map-get-entry map fresh-key 'foo) (entry fresh-key 'foo))
        (check-equal? (sorted-map-get-entry map fresh-key (λ () 'foo)) (entry fresh-key 'foo)))

      (test-case "sorted-map-least-key"
        (cond
          [(sorted-map-empty? map)
           (check-equal? (sorted-map-least-key map) absent)]
          [else
           (define first-key (sequence-ref (in-sorted-map-keys map) 0))
           (check-equal? (sorted-map-least-key map) (present first-key))]))

      (test-case "sorted-map-least-entry"
        (cond
          [(sorted-map-empty? map)
           (check-equal? (sorted-map-least-entry map) absent)]
          [else
           (define first-entry (sequence-ref (in-sorted-map map) 0))
           (check-equal? (sorted-map-least-entry map) (present first-entry))]))

      (test-case "sorted-map-greatest-key"
        (cond
          [(sorted-map-empty? map)
           (check-equal? (sorted-map-greatest-key map) absent)]
          [else
           (define last-key (sequence-ref (in-sorted-map-keys map #:descending? #true) 0))
           (check-equal? (sorted-map-greatest-key map) (present last-key))]))

      (test-case "sorted-map-greatest-entry"
        (cond
          [(sorted-map-empty? map)
           (check-equal? (sorted-map-greatest-entry map) absent)]
          [else
           (define last-entry (sequence-ref (in-sorted-map map #:descending? #true) 0))
           (check-equal? (sorted-map-greatest-entry map) (present last-entry))]))

      (test-case "sorted-map-key-less-than"
        (for/fold ([previous absent] #:result (void))
                  ([next (in-sorted-map-keys map)])
          (with-check-info (['upper-bound next])
            (check-equal? (sorted-map-key-less-than map next) previous)
            (present next))))

      (test-case "sorted-map-entry-less-than"
        (for/fold ([previous absent] #:result (void))
                  ([next (in-sorted-map map)])
          (with-check-info (['upper-bound next])
            (check-equal? (sorted-map-entry-less-than map (entry-key next)) previous)
            (present next))))

      (test-case "sorted-map-key-greater-than"
        (for/fold ([previous absent] #:result (void))
                  ([next (in-sorted-map-keys map #:descending? #true)])
          (with-check-info (['lower-bound next])
            (check-equal? (sorted-map-key-greater-than map next) previous)
            (present next))))

      (test-case "sorted-map-entry-greater-than"
        (for/fold ([previous absent] #:result (void))
                  ([next (in-sorted-map map #:descending? #true)])
          (with-check-info (['lower-bound next])
            (check-equal? (sorted-map-entry-greater-than map (entry-key next)) previous)
            (present next))))

      (test-case "sorted-map-key-at-most"
        (for ([key (in-sorted-map-keys map)])
          (with-check-info (['upper-bound key])
            (check-equal? (sorted-map-key-at-most map key) (present key)))))

      (test-case "sorted-map-entry-at-most"
        (for ([e (in-sorted-map map)])
          (define key (entry-key e))
          (with-check-info (['upper-bound key])
            (check-equal? (sorted-map-entry-at-most map key) (present e)))))

      (test-case "sorted-map-key-at-least"
        (for ([key (in-sorted-map-keys map)])
          (with-check-info (['lower-bound key])
            (check-equal? (sorted-map-key-at-least map key) (present key)))))

      (test-case "sorted-map-entry-at-least"
        (for ([e (in-sorted-map map)])
          (define key (entry-key e))
          (with-check-info (['upper-bound key])
            (check-equal? (sorted-map-entry-at-least map key) (present e)))))

      (test-case "sorted-map-size on exclusively bounded-above submaps"
        (for ([key (in-sorted-map-keys map)]
              [i (in-naturals)])
          (define submap (sorted-submap map (less-than-range key #:comparator key<=>)))
          (check-equal? (sorted-map-size submap) i)))

      (test-case "sorted-map-size on exclusively bounded-below submaps"
        (for ([key (in-sorted-map-keys map #:descending? #true)]
              [i (in-naturals)])
          (define submap (sorted-submap map (greater-than-range key #:comparator key<=>)))
          (check-equal? (sorted-map-size submap) i)))

      (test-case "sorted-map-size on inclusively bounded-above submaps"
        (for ([key (in-sorted-map-keys map)]
              [i (in-naturals 1)])
          (define submap (sorted-submap map (at-most-range key #:comparator key<=>)))
          (check-equal? (sorted-map-size submap) i)))

      (test-case "sorted-map-size on inclusively bounded-below submaps"
        (for ([key (in-sorted-map-keys map #:descending? #true)]
              [i (in-naturals 1)])
          (define submap (sorted-submap map (at-least-range key #:comparator key<=>)))
          (check-equal? (sorted-map-size submap) i)))))

  (test-case "persistent sorted map"

    (test-case "empty persistent sorted map"
      (test-sorted-map (sorted-map #:key-comparator natural<=>)))
    
    (test-case "singleton persistent sorted map"
      (test-sorted-map (sorted-map 1 'a #:key-comparator natural<=>)))

    (define map
      (sorted-map 1 'a 2 'b 3 'c 4 'd 5 'e 6 'f 7 'g 8 'h 9 'i 10 'j #:key-comparator real<=>))

    (test-sorted-map map)

    (test-case "reversed persistent sorted map"
      (test-sorted-map (sorted-map-reverse map)))

    (test-case "persistent sorted submap"
      (test-sorted-map (sorted-submap map (closed-range 3 8))))

    (test-case "reversed persistent sorted submap"

      (test-case "submap before reversal"
        (define key-range (closed-range 3 8))
        (with-check-info (['key-range key-range])
          (test-sorted-map (sorted-map-reverse (sorted-submap map key-range)))))

      (test-case "submap after reversal"
        (define key-range (closed-range 8 3 #:comparator (comparator-reverse real<=>)))
        (with-check-info (['key-range key-range])
          (test-sorted-map (sorted-submap (sorted-map-reverse map) key-range))))))

  (test-case "regular immutable sorted map"

    (define map
      (make-regular-immutable-sorted-map
       (list
        (entry 1 'a)
        (entry 2 'b)
        (entry 3 'c)
        (entry 4 'd)
        (entry 5 'e)
        (entry 6 'f)
        (entry 7 'g)
        (entry 8 'h)
        (entry 9 'i)
        (entry 10 'j))
       real<=>))

    (test-sorted-map map)

    (test-case "reversed immutable sorted map"
      (test-sorted-map (sorted-map-reverse map)))

    (test-case "immutable sorted submap"
      (define key-range (closed-range 3 8))
      (with-check-info (['key-range key-range])
        (test-sorted-map (sorted-submap map key-range))))

    (test-case "reversed immutable sorted submap"

      (test-case "submap before reversal"
        (define key-range (closed-range 3 8))
        (with-check-info (['key-range key-range])
          (test-sorted-map (sorted-map-reverse (sorted-submap map key-range)))))

      (test-case "submap after reversal"
        (define key-range (closed-range 8 3 #:comparator (comparator-reverse real<=>)))
        (with-check-info (['key-range key-range])
          (test-sorted-map (sorted-submap (sorted-map-reverse map) key-range)))))))
