#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [taking-local-maxima (->* () (comparator? #:key (-> any/c any/c)) transducer?)]
  [taking-local-minima (->* () (comparator? #:key (-> any/c any/c)) transducer?)]))


(require racket/match
         rebellion/base/comparator
         rebellion/base/option
         rebellion/base/variant
         rebellion/streaming/transducer/base)


;@----------------------------------------------------------------------------------------------------


(struct local-maxima-consumption-state (previous-element previous-key ascending?) #:transparent)
(struct local-maxima-emission-state (emission-element previous-element previous-key) #:transparent)


(define (taking-local-maxima [comparator real<=>]
                             #:key [key-function values]
                             #:name [name 'taking-local-maxima])

  (define (start)
    (variant #:consume absent))

  (define (consume state element)
    (match state

      [(== absent)
       (variant
        #:consume (present (local-maxima-consumption-state element (key-function element) #true)))]
      
      [(present (local-maxima-consumption-state previous-element previous-key #true))
       (define key (key-function element))
       (match (compare comparator key previous-key)
         [(== lesser) (variant #:emit (local-maxima-emission-state previous-element element key))]
         [_ (variant #:consume (present (local-maxima-consumption-state element key #true)))])]
      
      [(present (local-maxima-consumption-state previous-element previous-key #false))
       (define key (key-function element))
       (define ascending?
       (match (compare comparator key previous-key)
         [(== greater) #true]
         [_ #false]))
       (variant #:consume (present (local-maxima-consumption-state element key ascending?)))]))

  (define (emit state)
    (match-define (local-maxima-emission-state emission-element previous-element key) state)
    (define next-state
      (variant #:consume (present (local-maxima-consumption-state previous-element key #false))))
    (emission next-state emission-element))

  (define (half-close state)
    (match state
      [(== absent) (variant #:finish #false)]
      [(present (local-maxima-consumption-state previous-element previous-key #true))
       (variant #:half-closed-emit previous-element)]
      [(present (local-maxima-consumption-state previous-element previous-key #false))
       (variant #:finish #false)]))

  (define (half-closed-emit element)
    (half-closed-emission (variant #:finish #false) element))
          
  (make-transducer
   #:starter start
   #:consumer consume
   #:emitter emit
   #:half-closer half-close
   #:half-closed-emitter half-closed-emit
   #:finisher void
   #:name name))


(define (taking-local-minima [comparator real<=>] #:key [key-function values])
  (taking-local-maxima
   (comparator-reverse comparator) #:key key-function #:name 'taking-local-minima))
