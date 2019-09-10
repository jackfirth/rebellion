#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [relative-position (-> (real-in 0 1) (real-in 0 1) relative-position?)]
  [relative-position? predicate/c]
  [relative-position-x (-> relative-position? (real-in 0 1))]
  [relative-position-y (-> relative-position? (real-in 0 1))]
  [sub-range-binder-vector
   (->* (#:source identifier?
         #:target identifier?
         #:target-start natural?
         #:target-length natural?)
        (#:source-start natural?
         #:source-length natural?
         #:source-arrow-position relative-position?
         #:target-arrow-position relative-position?)
        immutable-vector?)]))

(require racket/math
         rebellion/collection/immutable-vector)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

;; We use a struct instead of rebellion/type to avoid circular dependencies.
(struct relative-position (x y) #:transparent)
(define center (relative-position 1/2 1/2))

;; Constructs an immutable vector of the correct shape for use with the
;; 'sub-range-binders syntax property. This function exists solely to provide
;; a more convenient interface to the vector construction, with named keyword
;; arguments and sensible defaults.
(define (sub-range-binder-vector
         #:source source-id
         #:target target-id
         #:target-start target-start
         #:target-length target-length
         #:source-start [source-start 0]
         #:source-length [source-length (identifier-length source-id)]
         #:source-arrow-position [source-position center]
         #:target-arrow-position [target-position center])
  (immutable-vector source-id
                    source-start
                    source-length
                    (relative-position-x source-position)
                    (relative-position-y source-position)
                    target-id
                    target-start
                    target-length
                    (relative-position-x target-position)
                    (relative-position-y target-position)))

(define (syntax-add-sub-range-binders stx . binders)
  (syntax-property stx 'sub-range-binders binders))

(define (identifier-length id)
  (string-length (symbol->string (syntax-e id))))

(module+ test
  (test-case "sub-range-binder-vector"
    (define here #'here)
    (define there #'there)
    (check-equal? (sub-range-binder-vector #:source here
                                           #:target there
                                           #:target-start 1
                                           #:target-length 4)
                  (immutable-vector here
                                    0
                                    4
                                    1/2
                                    1/2
                                    there
                                    1
                                    4
                                    1/2
                                    1/2))))
