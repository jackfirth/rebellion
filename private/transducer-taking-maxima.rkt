#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [taking-maxima (->* () (comparator? #:key (-> any/c any/c)) transducer?)]
  [taking-minima (->* () (comparator? #:key (-> any/c any/c)) transducer?)]))

(require racket/match
         rebellion/base/comparator
         rebellion/base/impossible-function
         rebellion/base/option
         rebellion/base/variant
         rebellion/collection/immutable-vector
         rebellion/collection/vector/builder
         rebellion/streaming/transducer/base
         rebellion/type/tuple)

;@------------------------------------------------------------------------------

(define-tuple-type maxima-consumption-state (builder representative))
(define-tuple-type maxima-emission-state (elements index))

(define (taking-maxima [comparator real<=>] #:key [key-function values])

  (define (start)
    (define state (maxima-consumption-state (make-vector-builder) absent))
    (variant #:consume state))

  (define (consume state element)
    (match-define (maxima-consumption-state builder representative) state)
    (match representative
      [(== absent)
       (define next-builder (vector-builder-add builder element))
       (define new-representative (present (key-function element)))
       (define next-state
         (maxima-consumption-state next-builder new-representative))
       (variant #:consume next-state)]
      [(present max-key)
       (define element-key (key-function element))
       (match (compare comparator element-key max-key)
         [(== lesser) (variant #:consume state)]
         [(== greater)
          (define new-builder
            (vector-builder-add (make-vector-builder) element))
          (define next-state
            (maxima-consumption-state new-builder (present element-key)))
          (variant #:consume next-state)]
         [(== equivalent)
          (define next-builder (vector-builder-add builder element))
          (define next-state
            (maxima-consumption-state next-builder representative))
          (variant #:consume next-state)])]))
  
  (define (half-close state)
    (define elements (build-vector (maxima-consumption-state-builder state)))
    (if (empty-immutable-vector? elements)
        (variant #:finish #false)
        (variant #:half-closed-emit (maxima-emission-state elements 0))))
  
  (define (half-closed-emitter state)
    (match-define (maxima-emission-state elements index) state)
    (define emitted (vector-ref elements index))
    (define next-index (add1 index))
    (define next-state
      (if (equal? next-index (vector-length elements))
          (variant #:finish #false)
          (variant
           #:half-closed-emit (maxima-emission-state elements next-index))))
    (half-closed-emission next-state emitted))
  
  (make-transducer
   #:starter start
   #:consumer consume
   #:emitter impossible
   #:half-closer half-close
   #:half-closed-emitter half-closed-emitter
   #:finisher void))

(define (taking-minima [comparator real<=>] #:key [key-function values])
  (taking-maxima (comparator-reverse comparator) #:key key-function))
