#lang racket/base


(module+ test
  (require racket/match
           rackunit
           rebellion/base/comparator
           rebellion/base/option
           rebellion/base/range
           rebellion/collection/entry
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
        (test-sorted-set (sorted-map-keys map)))))

  (test-case "immutable sorted map"
    
    (test-case "empty immutable sorted map"
      (test-sorted-map (sorted-map #:key-comparator natural<=>)))

    (test-case "singleton immutable sorted map"
      (test-sorted-map (sorted-map 1 'a #:key-comparator natural<=>)))

    (define map
      (sorted-map 1 'a 2 'b 3 'c 4 'd 5 'e 6 'f 7 'g 8 'h 9 'i 10 'j #:key-comparator real<=>))
    (test-sorted-map map)

    (test-case "reversed immutable sorted map"
      (test-sorted-map (sorted-map-reverse map)))))
