#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [taking-duplicates (->* () (#:key (-> any/c any/c)) transducer?)]))

(require racket/set
         rebellion/base/impossible-function
         rebellion/base/variant
         rebellion/private/static-name
         rebellion/streaming/transducer/base)

;@----------------------------------------------------------------------------------------------------

(define/name (taking-duplicates #:key [key-function values])

  (define (start) (variant #:consume (set)))

  (define (consume state element)
    (define key (key-function element))
    (if (set-member? state key)
        (variant #:emit (emission (variant #:consume state) element))
        (variant #:consume (set-add state key))))

  (define (emit state) state)

  (define (half-close state) (variant #:finish #false))

  (make-transducer
   #:starter start
   #:consumer consume
   #:emitter emit
   #:half-closer half-close
   #:half-closed-emitter impossible
   #:finisher void
   #:name enclosing-function-name))
