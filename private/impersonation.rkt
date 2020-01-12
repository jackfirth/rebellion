#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [impersonator-property-hash/c flat-contract?]
  [function-impersonate
   (->* (procedure?)
        (#:guard (or/c procedure? #f)
         #:properties impersonator-property-hash/c
         #:application-marks (and/c hash? immutable?)
         #:chaperone? boolean?)
        procedure?)]
  [struct-impersonate
   (->* (any/c struct-type?)
        (#:properties impersonator-property-hash/c #:chaperone? boolean?)
        any/c)]))

(require racket/bool)

;@------------------------------------------------------------------------------

(define impersonator-property-hash/c
  (hash/c impersonator-property? any/c #:immutable #t #:flat? #t))

(define (impersonator-properties->positional-arguments props)
  (for*/list ([(k v) (in-immutable-hash props)]
              [element (in-list (list k v))])
    element))

(define (function-impersonate function
                              #:guard [guard #f]
                              #:properties [props (hash)]
                              #:application-marks [marks (hash)]
                              #:chaperone? [chaperone? (false? guard)])
  (define impersonator-factory
    (if chaperone? chaperone-procedure impersonate-procedure))
  (define prop-args (impersonator-properties->positional-arguments props))
  (define mark-args
    (for*/list
        ([(k v) (in-immutable-hash props)]
         [element
          (in-list (list impersonator-prop:application-mark (cons k v)))])
      element))
  (apply impersonator-factory function guard (append prop-args mark-args)))

(define (struct-impersonate instance type
                            #:properties [props (hash)]
                            #:chaperone? [chaperone? #t])
  (define impersonator-factory
    (if chaperone? chaperone-struct impersonate-struct))
  (define prop-args (impersonator-properties->positional-arguments props))
  (apply impersonator-factory instance type prop-args))
