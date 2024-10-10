#lang racket/base


(require racket/contract/base)


(provide entry)


(provide
 (contract-out
  [entry? (-> any/c boolean?)]
  [entry-key (-> entry? any/c)]
  [entry-value (-> entry? any/c)]
  [entry/c (-> contract? contract? contract?)]
  [bisecting (-> (-> any/c any/c) (-> any/c any/c) (transducer/c any/c entry?))]
  [mapping-keys (-> (-> any/c any/c) (transducer/c entry? entry?))]
  [mapping-values (-> (-> any/c any/c) (transducer/c entry? entry?))]
  [indexing (-> (-> any/c any/c) (transducer/c any/c entry?))]
  [filtering-keys (-> (-> any/c boolean?) (transducer/c entry? entry?))]
  [filtering-values (-> (-> any/c boolean?) (transducer/c entry? entry?))]
  [append-mapping-keys
   (-> (-> any/c (sequence/c any/c)) (transducer/c entry? entry?))]
  [append-mapping-values
   (-> (-> any/c (sequence/c any/c)) (transducer/c entry? entry?))]
  [batching-into-entries (transducer/c any/c entry?)]
  [grouping (-> reducer? (transducer/c entry? entry?))]))


(require guard
         racket/contract/combinator
         racket/match
         racket/sequence
         racket/set
         rebellion/base/impossible-function
         rebellion/base/option
         rebellion/base/variant
         rebellion/collection/list
         rebellion/private/contract-projection
         rebellion/private/static-name
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


(define (entry/c key-contract* value-contract*)
  (define key-contract (coerce-contract 'entry/c key-contract*))
  (define value-contract (coerce-contract 'entry/c value-contract*))
  (define name (build-compound-type-name 'entry/c key-contract value-contract))
  (define (late-neg blame)
    (projection-and (contract-get-projection entry? blame)
                    (entry-projection key-contract value-contract blame)))
  (define maker
    (if (and (flat-contract? key-contract) (flat-contract? value-contract))
        make-flat-contract
        make-chaperone-contract))
  (maker #:name name #:late-neg-projection late-neg))


(define (entry-projection key-contract value-contract blame)
  (define underlying-key-projection
    (contract-get-projection key-contract (blame-add-context blame "the key field of the entry in")))
  (define underlying-value-projection
    (contract-get-projection
     value-contract (blame-add-context blame "the value field of the entry in")))
  (λ (e missing-party)
    (entry (underlying-key-projection (entry-key e) missing-party)
           (underlying-value-projection (entry-value e) missing-party))))


(define (bisecting key-function value-function)
  (mapping
   (λ (element) (entry (key-function element) (value-function element)))))


(define (mapping-keys key-function)
  (mapping (λ (e)
             (match-define (entry k v) e)
             (entry (key-function k) v))))


(define (mapping-values value-function)
  (mapping (λ (e)
             (match-define (entry k v) e)
             (entry k (value-function v)))))


(define (indexing key-function) (bisecting key-function values))


(define (filtering-keys key-predicate)
  (filtering (λ (e) (key-predicate (entry-key e)))))


(define (filtering-values value-predicate)
  (filtering (λ (e) (value-predicate (entry-value e)))))


(define (append-mapping-keys key-sequence-maker)
  (append-mapping
   (λ (e)
     (match-define (entry k v) e)
     (sequence-map (λ (k2) (entry k2 v)) (key-sequence-maker k)))))


(define (append-mapping-values value-sequence-maker)
  (append-mapping
   (λ (e)
     (match-define (entry k v) e)
     (sequence-map (λ (v2) (entry k v2)) (value-sequence-maker v)))))


(module+ test
  (test-case "entry pattern matching"
    (check-match (entry 'a 42) (entry _ _)))

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


(define batching-into-entries-message
  "odd number of sequence elements
 last key could not be paired with a value")


(define/name batching-into-entries
  (make-transducer
   #:starter (λ () (variant #:consume absent))
   #:consumer
   (λ (previous element)
     (option-case previous
                  #:present (λ (key) (variant #:emit (entry key element)))
                  #:absent (λ () (variant #:consume (present element)))))
   #:emitter (λ (e) (emission (variant #:consume absent) e))
   #:half-closer
   (λ (previous)
     (option-case previous
                  #:present
                  (λ (key)
                    (raise-arguments-error
                     (name batching-into-entries)
                     batching-into-entries-message
                     "last key" key))
                  #:absent
                  (λ () (variant #:finish #f))))
   #:half-closed-emitter impossible
   #:finisher void
   #:name enclosing-variable-name))


(module+ test
  (test-case (name-string batching-into-entries)
    (test-case "should turn even-length sequence into entries"
      (define actual
        (transduce (list 'a 1 'b 2 'c 3)
                   batching-into-entries
                   #:into into-list))
      (check-equal? actual (list (entry 'a 1) (entry 'b 2) (entry 'c 3))))

    (test-case "should raise error on odd-length sequence"
      (define (actual)
        (transduce (list 'a 1 'b 2 'c 3 'd)
                   batching-into-entries
                   #:into into-list))
      (check-exn exn:fail:contract? actual)
      (check-exn #rx"last key: 'd" actual))))


(define-record-type groups (reducer-states reverse-ordered-keys finished-keys))
(define-record-type closing-groups (reducer-states encounter-ordered-keys size))
(define-record-type group-emission (key value state))


(define (make-empty-groups)
  (groups #:reducer-states (make-hash)
          #:reverse-ordered-keys (list)
          #:finished-keys (set)))


(define/guard (groups-insert g k v #:reducer value-reducer)
  (define starter (reducer-starter value-reducer))
  (define consumer (reducer-consumer value-reducer))
  (define early-finisher (reducer-early-finisher value-reducer))
  (define states (groups-reducer-states g))
  (define keys (groups-reverse-ordered-keys g))
  (define finished (groups-finished-keys g))
  (guard (not (set-member? finished k)) #:else
    g)

  (guard (not (hash-has-key? states k)) #:else
    (define value-state (consumer (hash-ref states k) v))
    (guard (variant-tagged-as? value-state '#:consume) #:else
      (hash-remove! states k)
      (define next-g
        (groups
         #:reducer-states states
         #:reverse-ordered-keys (remove k keys)
         #:finished-keys (set-add finished k)))
      (group-emission
       #:key k
       #:value (early-finisher (variant-value value-state))
       #:state next-g))
    (hash-set! states k (variant-value value-state))
    g)
    
  (define value-state (starter))
  (guard (variant-tagged-as? value-state '#:early-finish) #:else
    (hash-set! states k (variant-value value-state))
    (define intermediate-g
      (groups
       #:reducer-states states
       #:reverse-ordered-keys (list-insert keys k)
       #:finished-keys finished))
    (groups-insert intermediate-g k v #:reducer value-reducer))
  (define next-g
    (groups
     #:reducer-states states
     #:reverse-ordered-keys keys
     #:finished-keys (set-add finished k)))
  (group-emission
   #:key k
   #:value (early-finisher (variant-value value-state))
   #:state next-g))


(define (half-close-groups g)
  (define keys (reverse (groups-reverse-ordered-keys g)))
  (closing-groups #:reducer-states (groups-reducer-states g)
                  #:encounter-ordered-keys keys
                  #:size (list-size keys)))


(define/name (grouping value-reducer)
  (define start-value (reducer-starter value-reducer))
  (define consume-value (reducer-consumer value-reducer))
  (define finish-value (reducer-finisher value-reducer))
  (define finish-value-early (reducer-early-finisher value-reducer))
  (make-transducer
   #:starter (λ () (variant #:consume (make-empty-groups)))
   #:consumer
   (λ (g e)
     (guarded-block
       (match-define (entry k v) e)
       (define next (groups-insert g k v #:reducer value-reducer))
       (guard (groups? next) #:else
         (variant #:emit next))
       (variant #:consume next)))
   #:emitter
   (λ (state)
     (emission (variant #:consume (group-emission-state state))
               (entry (group-emission-key state)
                      (group-emission-value state))))
   #:half-closer
   (λ (g*)
     (guarded-block
       (define g (half-close-groups g*))
       (guard (positive? (closing-groups-size g)) #:else
         (variant #:finish #false))
       (variant #:half-closed-emit g)))
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
       (if (zero? (closing-groups-size next-g))
           (variant #:finish #false)
           (variant #:half-closed-emit next-g)))
     (half-closed-emission next-state (entry next-key next-value)))
   #:finisher void
   #:name enclosing-function-name))


(module+ test
  (test-case (name-string grouping)
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
