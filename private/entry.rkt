#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [entry (-> any/c any/c entry?)]
  [entry? (-> any/c boolean?)]
  [entry-key (-> entry? any/c)]
  [entry-value (-> entry? any/c)]
  [bisecting (-> (-> any/c any/c) (-> any/c any/c) transducer?)]
  [mapping-keys (-> (-> any/c any/c) transducer?)]
  [mapping-values (-> (-> any/c any/c) transducer?)]
  [indexing (-> (-> any/c any/c) transducer?)]
  [filtering-keys (-> predicate/c transducer?)]
  [filtering-values (-> predicate/c transducer?)]
  [append-mapping-keys (-> (-> any/c (sequence/c any/c)) transducer?)]
  [append-mapping-values (-> (-> any/c (sequence/c any/c)) transducer?)]
  [grouping (-> reducer? transducer?)]))

(require racket/sequence
         racket/set
         rebellion/base/variant
         rebellion/collection/list
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/record
         rebellion/type/tuple)

(module+ test
  (require (submod "..")
           rackunit
           rebellion/base/option))

;@------------------------------------------------------------------------------

(define-tuple-type entry (key value))

(define (bisecting key-function value-function)
  (mapping
   (λ (element) (entry (key-function element) (value-function element)))))

(define (mapping-keys key-function)
  (mapping (λ (e) (entry (key-function (entry-key e)) (entry-value e)))))

(define (mapping-values value-function)
  (mapping (λ (e) (entry (entry-key e) (value-function (entry-value e))))))

(define (indexing key-function) (bisecting key-function values))

(define (filtering-keys key-predicate)
  (filtering (λ (e) (key-predicate (entry-key e)))))

(define (filtering-values value-predicate)
  (filtering (λ (e) (value-predicate (entry-value e)))))

(define (append-mapping-keys key-sequence-maker)
  (append-mapping
   (λ (e)
     (define v (entry-value e))
     (sequence-map (λ (k) (entry k v)) (key-sequence-maker (entry-key e))))))

(define (append-mapping-values value-sequence-maker)
  (append-mapping
   (λ (e)
     (define k (entry-key e))
     (sequence-map (λ (v) (entry k v))
                   (value-sequence-maker (entry-value e))))))

(module+ test
  (test-case "bisecting"
    (check-equal? (transduce (list "the" "quick" "brown" "fox")
                             (bisecting string->symbol string-length)
                             #:into into-list)
                  (list (entry 'the 3)
                        (entry 'quick 5)
                        (entry 'brown 5)
                        (entry 'fox 3))))
  
  (test-case "mapping-keys"
    (check-equal? (transduce (list (entry "foo" 1) (entry "bar" 2))
                             (mapping-keys string->symbol)
                             #:into into-list)
                  (list (entry 'foo 1) (entry 'bar 2))))
  
  (test-case "mapping-values"
    (check-equal? (transduce (list (entry 'foo 1) (entry 'bar 2))
                             (mapping-values (λ (x) (* x 2)))
                             #:into into-list)
                  (list (entry 'foo 2) (entry 'bar 4))))
  
  (test-case "indexing"
    (check-equal? (transduce (list "the" "quick" "brown" "fox")
                             (indexing string-length)
                             #:into into-list)
                  (list (entry 3 "the")
                        (entry 5 "quick")
                        (entry 5 "brown")
                        (entry 3 "fox"))))
  
  (test-case "filtering-keys"
    (check-equal? (transduce (list (entry 1 'foo) (entry 2 'bar) (entry 3 'baz))
                             (filtering-keys even?)
                             #:into into-list)
                  (list (entry 2 'bar))))
  
  (test-case "filtering-values"
    (check-equal? (transduce (list (entry 'foo 1) (entry 'bar 2) (entry 'baz 3))
                             (filtering-values odd?)
                             #:into into-list)
                  (list (entry 'foo 1) (entry 'baz 3))))

  (test-case "append-mapping-keys"
    (check-equal? (transduce (list (entry (list 1 2 3) 'foo)
                                   (entry (list 4 5 6) 'bar))
                             (append-mapping-keys in-list)
                             #:into into-list)
                  (list (entry 1 'foo)
                        (entry 2 'foo)
                        (entry 3 'foo)
                        (entry 4 'bar)
                        (entry 5 'bar)
                        (entry 6 'bar))))

  (test-case "append-mapping-values"
    (check-equal? (transduce (list (entry 'foo (list 1 2 3))
                                   (entry 'bar (list 4 5 6)))
                             (append-mapping-values in-list)
                             #:into into-list)
                  (list (entry 'foo 1)
                        (entry 'foo 2)
                        (entry 'foo 3)
                        (entry 'bar 4)
                        (entry 'bar 5)
                        (entry 'bar 6)))))

(define-record-type groups (reducer-states reverse-ordered-keys finished-keys))
(define-record-type closing-groups (reducer-states encounter-ordered-keys size))
(define-record-type group-emission (key value state))

(define (make-empty-groups)
  (groups #:reducer-states (make-hash)
          #:reverse-ordered-keys (list)
          #:finished-keys (set)))

(define (groups-insert g k v #:reducer value-reducer)
  (define starter (reducer-starter value-reducer))
  (define consumer (reducer-consumer value-reducer))
  (define early-finisher (reducer-early-finisher value-reducer))
  (define states (groups-reducer-states g))
  (define keys (groups-reverse-ordered-keys g))
  (define finished (groups-finished-keys g))
  (cond
    [(set-member? finished k) g]
    [(hash-has-key? states k)
     (define value-state (consumer (hash-ref states k) v))
     (cond
       [(variant-tagged-as? value-state '#:consume)
        (hash-set! states k (variant-value value-state))
        g]
       [else
        (hash-remove! states k)
        (define next-g
          (groups #:reducer-states states
                  #:reverse-ordered-keys (remove k keys)
                  #:finished-keys (set-add finished k)))
        (group-emission #:key k
                        #:value (early-finisher (variant-value value-state))
                        #:state next-g)])]
    [else
     (define value-state (starter))
     (cond
       [(variant-tagged-as? value-state '#:early-finish)
        (define next-g
          (groups #:reducer-states states
                  #:reverse-ordered-keys keys
                  #:finished-keys (set-add finished k)))
        (group-emission #:key k
                        #:value (early-finisher (variant-value value-state))
                        #:state next-g)]
       [else
        (hash-set! states k (variant-value value-state))
        (define intermediate-g
          (groups #:reducer-states states
                  #:reverse-ordered-keys (list-insert keys k)
                  #:finished-keys finished))
        (groups-insert intermediate-g k v #:reducer value-reducer)])]))

(define (half-close-groups g)
  (define keys (reverse (groups-reverse-ordered-keys g)))
  (closing-groups #:reducer-states (groups-reducer-states g)
                  #:encounter-ordered-keys keys
                  #:size (list-size keys)))

(define (grouping value-reducer)
  (define start-value (reducer-starter value-reducer))
  (define consume-value (reducer-consumer value-reducer))
  (define finish-value (reducer-finisher value-reducer))
  (define finish-value-early (reducer-early-finisher value-reducer))
  (make-transducer
   #:starter (λ () (variant #:consume (make-empty-groups)))
   #:consumer
   (λ (g e)
     (define next
       (groups-insert g (entry-key e) (entry-value e) #:reducer value-reducer))
     (cond
       [(groups? next) (variant #:consume next)]
       [else (variant #:emit next)]))
   #:emitter
   (λ (state)
     (emission (variant #:consume (group-emission-state state))
               (entry (group-emission-key state)
                      (group-emission-value state))))
   #:half-closer
   (λ (g*)
     (define g (half-close-groups g*))
     (cond
       [(zero? (closing-groups-size g)) (variant #:finish #f)]
       [else (variant #:half-closed-emit g)]))
   #:half-closed-emitter
   (λ (g)
     (define reducer-states (closing-groups-reducer-states g))
     (define keys (closing-groups-encounter-ordered-keys g))
     (define next-key (list-first keys))
     (define next-value (finish-value (hash-ref reducer-states next-key)))
     (hash-remove! reducer-states next-key)
     (define next-g
       (closing-groups
        #:reducer-states reducer-states
        #:encounter-ordered-keys (list-rest keys)
        #:size (sub1 (closing-groups-size g))))
     (define next-state
       (cond
         [(zero? (closing-groups-size next-g)) (variant #:finish #f)]
         [else (variant #:half-closed-emit next-g)]))
     (half-closed-emission next-state (entry next-key next-value)))
   #:finisher void))

(module+ test
  (test-case "grouping"
    (check-equal? (transduce (list (entry 'a 1)
                                   (entry 'a 2)
                                   (entry 'a 3)
                                   (entry 'b 4)
                                   (entry 'b 5))
                             (grouping into-sum)
                             #:into into-list)
                  (list (entry 'a 6) (entry 'b 9)))
    (check-equal? (transduce (list (entry 'a 1)
                                   (entry 'a 2)
                                   (entry 'a 3)
                                   (entry 'b 4)
                                   (entry 'b 5))
                             (grouping (into-nth 0))
                             #:into into-list)
                  (list (entry 'a (present 1))
                        (entry 'b (present 4))))
    (check-equal? (transduce (list (entry 'a 1)
                                   (entry 'b 5)
                                   (entry 'a 3)
                                   (entry 'b 4)
                                   (entry 'a 2))
                             (grouping (into-max))
                             #:into into-list)
                  (list (entry 'a (present 3))
                        (entry 'b (present 5))))))
