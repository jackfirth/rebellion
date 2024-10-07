#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [reducer-zip
   (->i
    #:chaperone
    ([zip-function procedure?])
    #:rest [reducers (listof reducer?)]
    #:pre/name (zip-function reducers)
    "zip function arity must match up with the number of reducers given"
    (procedure-arity-includes? zip-function (length reducers))
    [_ reducer?])]))


(require rebellion/base/variant
         rebellion/private/static-name
         rebellion/streaming/reducer/private/base
         rebellion/type/record)


;@----------------------------------------------------------------------------------------------------


(define-record-type zip-state (substate-values finished-reducers))


(define/name (reducer-zip zip-function . reducers)
  (define reducer-count (length reducers))
  (define starters
    (vector->immutable-vector
     (for/vector #:length reducer-count ([reducer (in-list reducers)]) (reducer-starter reducer))))
  (define consumers
    (vector->immutable-vector
     (for/vector #:length reducer-count ([reducer (in-list reducers)]) (reducer-consumer reducer))))
  (define finishers
    (vector->immutable-vector
     (for/vector #:length reducer-count ([reducer (in-list reducers)]) (reducer-finisher reducer))))
  (define early-finishers
    (vector->immutable-vector
     (for/vector #:length reducer-count
       ([reducer (in-list reducers)])
       (reducer-early-finisher reducer))))

  (define (tag-state state)
    (if (for/and ([finished-early? (in-vector (zip-state-finished-reducers state))]) finished-early?)
        (variant #:early-finish state)
        (variant #:consume state)))

  (define (start)
    (define substates (for/vector ([starter (in-vector starters)]) (starter)))
    (define substate-values (for/vector ([substate (in-vector substates)]) (variant-value substate)))
    (define finished-reducers
      (for/vector ([substate (in-vector substates)]) (variant-tagged-as? substate '#:early-finish)))
    (tag-state (zip-state #:substate-values substate-values #:finished-reducers finished-reducers)))

  (define (consume state element)
    (define substates (zip-state-substate-values state))
    (define finished (zip-state-finished-reducers state))
    (for ([i (in-range 0 reducer-count)]
          #:unless (vector-ref finished i))
      (define substate (vector-ref substates i))
      (define next-substate ((vector-ref consumers i) substate element))
      (vector-set! substates i (variant-value next-substate))
      (vector-set! finished i (variant-tagged-as? next-substate '#:early-finish)))
    (tag-state state))

  (define (finish state)
    (define substates (zip-state-substate-values state))
    (define finished (zip-state-finished-reducers state))
    (define results
      (for/list ([i (in-range 0 reducer-count)])
        (define substate (vector-ref substates i))
        (define finish-function (vector-ref (if (vector-ref finished i) early-finishers finishers) i))
        (finish-function substate)))
    (apply zip-function results))

  (define (finish-early state)
    (define substates (zip-state-substate-values state))
    (define results
      (for/list ([i (in-range 0 reducer-count)])
        (define substate (vector-ref substates i))
        (define finish-function (vector-ref early-finishers i))
        (finish-function substate)))
    (apply zip-function results))

  (make-reducer
   #:starter start
   #:consumer consume
   #:finisher finish
   #:early-finisher finish-early
   #:name 'zipped))
