#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [transduce (-> sequence? #:into reducer? transducer? ... any/c)]
  [in-transduced (-> sequence? transducer? sequence?)]
  [mapping (-> (-> any/c any/c) transducer?)]
  [peeking (-> (-> any/c void?) transducer?)]
  [filtering (-> predicate/c transducer?)]
  [folding (-> (-> any/c any/c any/c) any/c transducer?)]
  [append-mapping (-> (-> any/c sequence?) transducer?)]
  [taking (-> natural? transducer?)]
  [taking-while (-> predicate/c transducer?)]
  [dropping (-> natural? transducer?)]
  [dropping-while (-> predicate/c transducer?)]))

(require racket/bool
         racket/list
         racket/math
         rebellion/base/option
         rebellion/base/pair
         rebellion/base/variant
         rebellion/collection/list
         rebellion/private/impossible
         rebellion/private/static-name
         rebellion/streaming/reducer
         rebellion/streaming/transducer/base
         rebellion/streaming/transducer/testing
         rebellion/type/record)

(module+ test
  (require (submod "..")
           racket/sequence
           racket/set
           rackunit))

;@------------------------------------------------------------------------------
;; Implementation of in-transduced
;;
;; The in-transduced function is the point of intergration between the
;; transducer API and racket's sequence API. It wraps a sequence with a
;; transducer and returns a new sequence that lazily transduces the original.
;;
;; Background
;;
;; The sequence API (as defined by make-do-sequence) uses a "position" to
;; represent the current state of the sequence iterator, but due to the way the
;; early-next-pos function is called a sequence can alternate between two
;; different state types. Let those types be called P and Q. Then the following
;; statements describe which sequence functions produce and consume which state
;; types. The function statements are listed in order of when they're called for
;; each output element of the sequence (i.e. the pos->element function is called
;; before early-next-pos and after continue-with-pos?).
;;
;; init-pos : P (I think? might be Q? seems like P is the only sensible choice)
;;
;; P -> continue-with-pos?
;; P -> pos->element (only if continue-with-pos? is true)
;; P -> early-next-pos -> Q
;; Q -> continue-after-pos+val?
;; Q -> next-pos -> P (only if continue-after-pos+val? is true)
;;
;; The continue-after-val? function is not relevant to in-transduced because the
;; current transduction state and the upstream sequence are the only things that
;; determine when the sequence ends, not the emitted values.
;;
;; Implementation Approach
;;
;; Positions are instances of transducer-position, which contains the current
;; transduction state, the most recent upstream sequence element (which is an
;; (or/c list? #f) value), and a thunk to generate rest of elements. The latter
;; two values are obtained from the sequence-generate* function. Whether the
;; position is a P-type position or a Q-type position affects whether the
;; position's transduction state is allowed to be a #:consume state. Q allows
;; #:consume, P does not.
;;
;; Valid P states: #:emit, #:half-closed-emit, #:finish
;; Valid Q states: #:emit, #:half-closed-emit, #:finish, #:consume
;;
;; Whenever a P is needed, as long as the current state is #:consume, upstream
;; sequence elements must be iterated. This occurs when producing the init-pos
;; and just after calling next-pos. These are the only points where upstream
;; sequence elements should be consumed. If the upstream sequence is exhausted,
;; then the transducer's half-closer function should be called on the #:consume
;; state in order to produce the next state. This transformation is performed by
;; the advance-transducer-position function.
;;
;; The returned sequence should produce one element each time the state is an
;; #:emit or #:half-closed-emit state. Once the state is #:finish, no more
;; elements should be produced and the transducer's finisher function should be
;; called on the final state. The sequence protocol uses the continue-with-pos?
;; and continue-after-pos+val? functions to stop iteration, but does not specify
;; any function to be called after iteration ends to clean things up. So, this
;; implementation will call the transducer's finisher function within the
;; continue-with-pos? and continue-after-pos+val? functions, just before they
;; return true.

(define-record-type transducer-position
  (state downstream-element upstream-element upstream-generator))

(define (make-initial-transducer-position trans seq)
  (define state ((transducer-starter trans)))
  (define-values (upstream-element upstream-generator) (sequence-generate* seq))
  (transducer-position #:state state
                       #:downstream-element absent
                       #:upstream-element upstream-element
                       #:upstream-generator upstream-generator))

(define (transducer-position-advance-repeatedly trans position)
  (if (variant-tagged-as? (transducer-position-state position) '#:consume)
      (transducer-position-advance-repeatedly
       trans
       (transducer-position-advance trans position))
      position))

(define (transducer-position-try-advance trans position)
  (if (variant-tagged-as? (transducer-position-state position) '#:consume)
      (transducer-position-advance trans position)
      position))

(define/name (transducer-position-advance trans position)
  (define consumer (transducer-consumer trans))
  (define half-closer (transducer-half-closer trans))
  (define state (transducer-position-state position))
  (define upstream-element (transducer-position-upstream-element position))
  (define upstream-generator (transducer-position-upstream-generator position))
  (cond
    [(false? upstream-element)
     (define next-state (half-closer (variant-value state)))
     (transducer-position #:state next-state
                          #:downstream-element absent
                          #:upstream-element upstream-element
                          #:upstream-generator upstream-generator)]
    [else
     (unless (equal? (length upstream-element) 1)
       (apply raise-result-arity-error
              enclosing-function-name
              1
              "\n  in: the transduced sequence"
              upstream-element))
     (define next-state
       (consumer (variant-value state) (first upstream-element)))
     (define-values (next-upstream-element next-upstream-generator)
       (upstream-generator))
     (transducer-position #:state next-state
                          #:downstream-element absent
                          #:upstream-element next-upstream-element
                          #:upstream-generator next-upstream-generator)]))

(define/name (transducer-position-try-emit trans position)
  (define emitter (transducer-emitter trans))
  (define half-closed-emitter (transducer-half-closed-emitter trans))
  (define state (transducer-position-state position))
  (define downstream-element (transducer-position-downstream-element position))
  (define upstream-element (transducer-position-upstream-element position))
  (define upstream-generator (transducer-position-upstream-generator position))
  (unless (absent? downstream-element)
    (raise-argument-error
     enclosing-function-name
     "transducer-position that is not already emitting an element downstream"
     position))
  (when (variant-tagged-as? state '#:consume)
    (raise-argument-error
     enclosing-function-name
     "cannot try emitting when in consuming position"
     position))
  (cond
    [(variant-tagged-as? state '#:emit)
     (define em (emitter (variant-value state)))
     (transducer-position #:state (emission-state em)
                          #:downstream-element (present (emission-value em))
                          #:upstream-element upstream-element
                          #:upstream-generator upstream-generator)]
    [(variant-tagged-as? state '#:half-closed-emit)
     (define em (half-closed-emitter (variant-value state)))
     (transducer-position
      #:state (half-closed-emission-state em)
      #:downstream-element (present (half-closed-emission-value em))
      #:upstream-element upstream-element
      #:upstream-generator upstream-generator)]
    [else
     ;; TODO: maybe this is where the finisher should be called?
     position]))

(define (continue-with-transducer-position? pos)
  (present? (transducer-position-downstream-element pos)))

(define (in-transduced seq trans)
  (define starter (transducer-starter trans))
  (define consumer (transducer-consumer trans))
  (define emitter (transducer-emitter trans))
  (define half-closer (transducer-half-closer trans))
  (define half-closed-emitter (transducer-half-closed-emitter trans))
  (define finisher (transducer-finisher trans))
  (make-do-sequence
   (λ ()
     (define init-pos (make-initial-transducer-position trans seq))
     (define init-emitting-pos
       (transducer-position-try-emit
        trans
        (transducer-position-advance-repeatedly trans init-pos)))
     (define continue-with-pos? continue-with-transducer-position?)
     (define (pos->element pos)
       (present-value (transducer-position-downstream-element pos)))
     (define (early-next-pos position)
       (define state (transducer-position-state position))
       (define upstream-element (transducer-position-upstream-element position))
       (define upstream-generator
         (transducer-position-upstream-generator position))
       (transducer-position #:state state
                            #:downstream-element absent
                            #:upstream-element upstream-element
                            #:upstream-generator upstream-generator))
     (define (next-pos position)
       (transducer-position-try-emit
        trans
        (transducer-position-advance-repeatedly trans position)))
     (define continue-after-pos+val? #f)
     (define continue-with-val? #f)
     (values pos->element
             early-next-pos
             next-pos
             init-emitting-pos
             continue-with-pos?
             continue-with-val?
             continue-after-pos+val?))))

(module+ test
  (test-case (name-string in-transduced)
    (define seq (in-transduced (in-range 5) (mapping number->string)))
    (check-equal? (sequence->list seq) (list "0" "1" "2" "3" "4"))
    (check-equal? (sequence->list seq) (sequence->list seq))
    (test-case "infinite"
      (define seq (in-transduced (in-naturals) (mapping number->string)))
      (check-equal? (sequence-ref seq 100) "100"))))

;@------------------------------------------------------------------------------
;; Implementation of into-transduced

;@------------------------------------------------------------------------------
;; Implementation of transducer-pipe

(define (transducer-pipe . transducers)
  ;; TODO: actually implement this
  (mapping values))

;@------------------------------------------------------------------------------
;; Implementation of transduce

(define (transduce seq #:into red . transducers)
  (reduce-all red
              (for/fold ([seq seq])
                        ([trans (in-list transducers)])
                (in-transduced seq trans))))

;@------------------------------------------------------------------------------
;; Everything else

(define stateless-consume (variant #:consume #f))
(define stateless-finish (variant #:finish #f))

(define (immediately-consuming-stateless-starter) stateless-consume)
(define (immediately-finishing-half-closer _) stateless-finish)

(define/name (mapping f)
  (make-transducer
   #:starter immediately-consuming-stateless-starter
   #:consumer (λ (_ v) (variant #:emit v))
   #:emitter (λ (v) (emission stateless-consume (f v)))
   #:half-closer immediately-finishing-half-closer
   #:half-closed-emitter impossible
   #:finisher void
   #:name enclosing-function-name))

(define/name (peeking handler)
  (make-transducer
   #:starter immediately-consuming-stateless-starter
   #:consumer (λ (_ v) (variant #:emit v))
   #:emitter (λ (v) (handler v) (emission stateless-consume v))
   #:half-closer immediately-finishing-half-closer
   #:half-closed-emitter impossible
   #:finisher void
   #:name enclosing-function-name))

(define/name (filtering pred)
  (define consume-state (variant #:consume #f))
  (make-transducer
   #:starter (λ () consume-state)
   #:consumer (λ (_ v) (if (pred v) (variant #:emit v) consume-state))
   #:emitter (λ (v) (emission consume-state v))
   #:half-closer (λ (_) (variant #:finish #f))
   #:half-closed-emitter impossible
   #:finisher void
   #:name enclosing-function-name))

(define/name (folding f init)
  (make-transducer
   #:starter (λ () (variant #:consume init))
   #:consumer (λ (st v) (variant #:emit (f st v)))
   #:emitter (λ (st) (emission (variant #:consume st) st))
   #:half-closer (λ (_) (variant #:finish #f))
   #:half-closed-emitter impossible
   #:finisher void
   #:name enclosing-function-name))

(define-record-type iterator (head tail-maker))

(define (sequence-iterate seq)
  (define-values (head tail-maker) (sequence-generate* seq))
  (iterator #:head head
            #:tail-maker tail-maker))

(define (empty-iterator? iter)
  (false? (iterator-head iter)))

(define (nonempty-iterator? iter)
  (list? (iterator-head iter)))

(define (iterator-tail iter)
  (define-values (head tail-maker) ((iterator-tail-maker iter)))
  (iterator #:head head #:tail-maker tail-maker))

(define/name (append-mapping f)
  (define consume-state (variant #:consume #f))
  (make-transducer
   #:starter (λ () consume-state)
   #:consumer (λ (_ v)
                (define it (sequence-iterate (f v)))
                (if (empty-iterator? it)
                    (variant #:consume #f)
                    (variant #:emit it)))
   #:emitter
   (λ (it)
     (define v (first (iterator-head it)))
     (define next-it (iterator-tail it))
     (define next-state
       (if (empty-iterator? next-it)
           consume-state
           (variant #:emit next-it)))
     (emission next-state v))
   #:half-closer (λ (_) (variant #:finish #f))
   #:half-closed-emitter impossible
   #:finisher void
   #:name enclosing-function-name))

(module+ test
  (test-case (name-string mapping)
    (define trans (mapping number->string))
    (define inputs (list 1 2 3))
    (define expected (list "1" "2" "3"))
    (check-equal? (transduce inputs trans #:into into-list) expected)
    (define expected-events
      (list start-event
            (consume-event 1)
            (emit-event "1")
            (consume-event 2)
            (emit-event "2")
            (consume-event 3)
            (emit-event "3")
            half-close-event
            finish-event))
    (check-equal? (transduce inputs (materializing trans) #:into into-list)
                  expected-events))

  (test-case (name-string peeking)
    (define peeked (mutable-set))
    (define trans (peeking (λ (v) (set-add! peeked v))))
    (define inputs (list 1 2 3))
    (define expected inputs)
    (check-equal? (transduce inputs trans #:into into-list) expected)
    (check-equal? peeked (mutable-set 1 2 3))
    (define expected-states
      (transduce inputs (materializing (mapping values)) #:into into-list))
    (check-equal? (transduce inputs (materializing trans) #:into into-list)
                  expected-states))
  
  (test-case (name-string filtering)
    (check-equal? (transduce (in-range 0 10) (filtering even?) #:into into-list)
                  (list 0 2 4 6 8)))
  
  (test-case (name-string folding)
    (check-equal? (transduce (in-range 0 10) (folding + 0) #:into into-list)
                  (list 0 1 3 6 10 15 21 28 36 45)))
  
  (test-case (name-string append-mapping)
    (check-equal? (transduce (list (vector 1 2 3)
                                   (vector 1 2 3 4 5)
                                   (vector 1 2))
                             (append-mapping in-vector)
                             #:into into-list)
                  (list 1 2 3 1 2 3 4 5 1 2))))

(define/name (taking amount)
  (make-transducer
   #:starter
   (if (zero? amount)
       (λ () stateless-finish)
       (λ () (variant #:consume amount)))
   #:consumer
   (λ (amount v)
     (define remaining (sub1 amount))
     (if (zero? remaining)
         (variant #:half-closed-emit v)
         (variant #:emit (pair remaining v))))
   #:emitter
   (λ (state)
     (define amount (pair-first state))
     (define v (pair-second state))
     (emission (variant #:consume amount) v))
   #:half-closer (λ (_) stateless-finish)
   #:half-closed-emitter (λ (v) (half-closed-emission stateless-finish v))
   #:finisher void
   #:name enclosing-function-name))

(define/name (dropping amount)
  (make-transducer
   #:starter (λ () (variant #:consume amount))
   #:consumer
   (λ (amount v)
     (if (zero? amount)
         (variant #:emit v)
         (variant #:consume (sub1 amount))))
   #:emitter
   (λ (v) (emission (variant #:consume 0) v))
   #:half-closer (λ (_) (variant #:finish #f))
   #:half-closed-emitter impossible
   #:finisher void
   #:name enclosing-function-name))

(define/name (taking-while pred)
  (make-transducer
   #:starter (λ () (variant #:consume #f))
   #:consumer (λ (_ v) (if (pred v) (variant #:emit v) (variant #:finish #f)))
   #:emitter (λ (v) (emission (variant #:consume #f) v))
   #:half-closer (λ (_) (variant #:finish #f))
   #:half-closed-emitter impossible
   #:finisher void
   #:name enclosing-function-name))

(define/name (dropping-while pred)
  (make-transducer
   #:starter (λ () (variant #:consume #f))
   #:consumer
   (λ (done? v)
     (if (or done? (not (pred v)))
         (variant #:emit v)
         (variant #:consume done?)))
   #:emitter
   (λ (v) (emission (variant #:consume #t) v))
   #:half-closer (λ (_) (variant #:finish #f))
   #:half-closed-emitter impossible
   #:finisher void
   #:name enclosing-function-name))

(module+ test
  (test-case (name-string taking)
    (check-equal? (transduce (in-naturals) (taking 5) #:into into-list)
                  (list 0 1 2 3 4))
    (check-equal? (transduce (in-range 10) (taking 0) #:into into-list)
                  empty-list)
    (check-equal? (transduce (in-range 10) (taking 25) #:into into-list)
                  (list 0 1 2 3 4 5 6 7 8 9)))
  
  (test-case (name-string dropping)
    (check-equal? (transduce (in-range 10) (dropping 3) #:into into-list)
                  (list 3 4 5 6 7 8 9))
    (check-equal? (transduce (in-range 10) (dropping 25) #:into into-list)
                  empty-list))
  
  (test-case (name-string taking-while)
    (define (small? n) (< n 5))
    (define while-small (taking-while small?))
    (check-equal? (transduce (in-naturals) while-small #:into into-list)
                  (list 0 1 2 3 4))
    (check-equal? (transduce (in-range 3) while-small #:into into-list)
                  (list 0 1 2))
    (check-equal? (transduce (list 0 1 2 'a 'b 3 4 'c 5 6 7)
                             (taking-while number?)
                             #:into into-list)
                  (list 0 1 2)))
  
  (test-case (name-string dropping-while)
    (check-equal? (transduce (list 0 1 2 'a 'b 3 4 'c 5 6 7)
                             (dropping-while number?)
                             #:into into-list)
                  (list 'a 'b 3 4 'c 5 6 7))
    (check-equal? (transduce (in-range 10)
                             (dropping-while number?)
                             #:into into-list)
                  empty-list)))
