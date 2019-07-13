#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [reducer? predicate/c]
  [make-fold-reducer
   (->* ((-> any/c any/c any/c) any/c)
        (#:name (or/c interned-symbol? #f))
        reducer?)]
  [make-effectful-fold-reducer
   (->* ((-> any/c any/c any/c) (-> any/c) (-> any/c any/c))
        (#:name (or/c interned-symbol? #f))
        reducer?)]
  [make-reducer
   (->* (#:starter (-> variant?)
         #:consumer (-> any/c any/c variant?)
         #:finisher (-> any/c any/c)
         #:early-finisher (-> any/c any/c))
        (#:name (or/c interned-symbol? #f))
        reducer?)]
  [reduce (-> reducer? any/c ... any/c)]
  [reduce-all (-> reducer? sequence? any/c)]
  [into-sum reducer?]
  [into-product reducer?]
  [into-count reducer?]))

(require racket/bool
         racket/list
         rebellion/base/symbol
         rebellion/base/variant
         rebellion/type/reference)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define-reference-type reducer (starter consumer finisher early-finisher))

(define (make-fold-reducer consumer init-state #:name [name #f])
  (make-effectful-fold-reducer consumer (λ () init-state) values #:name name))

(define (make-effectful-fold-reducer consumer init-state-maker finisher
                                     #:name [name #f])
  (make-reducer #:starter (λ () (variant #:consume (init-state-maker)))
                #:consumer (λ (s v) (variant #:consume (consumer s v)))
                #:finisher finisher
                #:early-finisher values
                #:name name))

(define into-sum (make-fold-reducer + 0 #:name 'into-sum))
(define into-product (make-fold-reducer * 1 #:name 'into-product))
(define into-count (make-fold-reducer (λ (s _) (add1 s)) 0 #:name 'into-count))

(define (reduce red . vs)
  (define starter (reducer-starter red))
  (define consumer (reducer-consumer red))
  (define finisher (reducer-finisher red))
  (define early-finisher (reducer-early-finisher red))
  (let loop ([tagged-state (starter)] [vs vs])
    (define state (variant-value tagged-state))
    (cond
      [(variant-tagged-as? tagged-state '#:early-finish)
       (early-finisher state)]
      [(empty? vs) (finisher state)]
      [else (loop (consumer state (first vs)) (rest vs))])))

(define (reduce-all red seq)
  (define starter (reducer-starter red))
  (define consumer (reducer-consumer red))
  (define finisher (reducer-finisher red))
  (define early-finisher (reducer-early-finisher red))
  (define-values (first-vs generate-rest) (sequence-generate* seq))
  (let loop ([tagged-state (starter)]
             [sequence-position 0]
             [first-vs first-vs]
             [generate-rest generate-rest])
    (define state (variant-value tagged-state))
    (cond
      [(variant-tagged-as? tagged-state '#:early-finish)
       (early-finisher state)]
      [(false? first-vs)
       (finisher state)]
      [(not (equal? (length first-vs) 1))
       (apply raise-result-arity-error
              'reduce-all 1
              (format "\n  in: sequence elements at position ~a"
                      sequence-position)
              first-vs)]
      [else
       (define-values (next-vs next-generate-rest) (generate-rest))
       (loop (consumer state (first first-vs))
             (add1 sequence-position)
             next-vs
             next-generate-rest)])))

(module+ test
  (test-case "reduce"
    (check-equal? (reduce into-sum 1 2 3 4 5) 15)
    (check-equal? (reduce into-product 2 3 5 7) 210)
    (check-equal? (reduce into-count 'a 'b 'c 'd) 4)
    (check-equal? (reduce into-sum) 0)
    (check-equal? (reduce into-product) 1)
    (check-equal? (reduce into-count) 0))
  (test-case "reduce-all"
    (check-equal? (reduce-all into-sum (in-range 6)) 15)
    (check-equal? (reduce-all into-product
                              (in-hash-values (hash 'a 2 'b 3 'c 5 'd 7)))
                  210)
    (check-equal? (reduce-all into-count (in-string "abcde")) 5)))
