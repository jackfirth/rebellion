#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [transducer-pipe (-> transducer? ... transducer?)]
  [transducer-compose (-> transducer? ... transducer?)]))

(require racket/contract/region
         rebellion/base/variant
         rebellion/collection/immutable-vector
         rebellion/streaming/transducer/base
         rebellion/type/record)

;@------------------------------------------------------------------------------
;; Implementation of transducer-pipe

;; Transducers should only be started once their output is requested by a
;; downstream transducer entering the consume state.

(define-record-type pipe-state (subtransducers))
(define-record-type subtransducer (transducer state))

(define (transducer-pipe . transducers-list)
  (define transducers (list->immutable-vector transducers-list))
  (define (start)
    (define state (make-start-pipe-state transducers))
    (cond
      [(final-pipe-state? state) (variant #:finish state)]
      [(emitting-pipe-state? state) (variant #:emit state)]
      [(half-closed-emitting-pipe-state? state)
       (variant #:half-closed-emit state)]
      [(consuming-pipe-state? state) (variant #:consume state)]
      [else (error "impossible")]))
  (define (consume state element) #f)
  (define (emit state) #f)
  (define (half-close state) #f)
  (define (half-closed-emit state) #f)
  (define (finish state) #f)
  (make-transducer
   #:starter start
   #:consumer consume
   #:emitter emit
   #:half-closer half-close
   #:half-closed-emitter half-closed-emit
   #:finisher finish
   #:name 'piped))

(define/contract (make-start-pipe-state transducers)
  (-> (vectorof transducer? #:immutable #t #:flat? #t) pipe-state?)
  (define subtransducers
    (for/vector #:length (vector-length transducers)
      ([trans (in-vector transducers)]
       [should-start? (in-sequences (list #t) (in-cycle (list #f)))])
      (subtransducer #:transducer trans
                     #:state (and should-start?
                                  ((transducer-starter trans))))))
  (pipe-state #:subtransducers subtransducers))

(define/contract (final-pipe-state? state)
  (-> pipe-state? boolean?)
  #f)

(define/contract (emitting-pipe-state? state)
  (-> pipe-state? boolean?)
  #f)

(define/contract (half-closed-emitting-pipe-state? state)
  (-> pipe-state? boolean?)
  #f)

(define/contract (consuming-pipe-state? state)
  (-> pipe-state? boolean?)
  #f)

(define (transducer-compose . transducers)
  (apply transducer-pipe (reverse transducers)))
