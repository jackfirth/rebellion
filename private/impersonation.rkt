#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [impersonator-property-hash/c flat-contract?]
  [function-impersonate
   (->* (procedure?)
        (#:arguments-guard (or/c procedure? #f)
         #:results-guard (or/c procedure? #f)
         #:properties impersonator-property-hash/c
         #:application-marks (and/c hash? immutable?)
         #:chaperone? boolean?)
        procedure?)]
  [struct-impersonate
   (->* (any/c struct-type?)
        (#:properties impersonator-property-hash/c #:chaperone? boolean?)
        any/c)]))

(require racket/bool
         rebellion/private/strict-cond)

(module+ test
  (require rackunit
           rebellion/private/static-name))

;@------------------------------------------------------------------------------

(define impersonator-property-hash/c
  (hash/c impersonator-property? any/c #:immutable #t #:flat? #t))

(define (impersonator-properties->positional-arguments props)
  (for*/list ([(k v) (in-immutable-hash props)]
              [element (in-list (list k v))])
    element))

(define (function-impersonate
         function
         #:arguments-guard [arguments-guard #f]
         #:results-guard [results-guard #f]
         #:properties [props (hash)]
         #:application-marks [marks (hash)]
         #:chaperone? [chaperone? (false? arguments-guard)])
  (define impersonator-factory
    (if chaperone? chaperone-procedure impersonate-procedure))
  (define prop-args (impersonator-properties->positional-arguments props))
  (define mark-args
    (for*/list
        ([(k v) (in-immutable-hash props)]
         [element
          (in-list (list impersonator-prop:application-mark (cons k v)))])
      element))
  (apply impersonator-factory
         function
         (function-application-guard arguments-guard results-guard)
         (append prop-args mark-args)))

(define (function-application-guard guard result-guard)
  (strict-cond
    [(false? result-guard) guard]
    [(false? guard) (λ xs (apply values (cons result-guard xs)))]
    [else
     (λ xs
       (call-with-values
        (λ () (apply guard xs))
        (λ guarded (apply values (cons result-guard guarded)))))]))
    

(define (struct-impersonate instance type
                            #:properties [props (hash)]
                            #:chaperone? [chaperone? #t])
  (define impersonator-factory
    (if chaperone? chaperone-struct impersonate-struct))
  (define prop-args (impersonator-properties->positional-arguments props))
  (apply impersonator-factory instance type prop-args))

(module+ test
  (test-case (name-string function-impersonate)
    (test-case "argument guard"
      (define arg1 (box #f))
      (define arg2 (box #f))
      (define (guard x y)
        (set-box! arg1 x)
        (set-box! arg2 y)
        (values x y))
      (define impersonated (function-impersonate expt #:arguments-guard guard))
      (check-equal? (impersonated 2 5) 32)
      (check-equal? (unbox arg1) 2)
      (check-equal? (unbox arg2) 5))

    (test-case "result guard"
      (define result (box #f))
      (define (guard x)
        (set-box! result x)
        x)
      (define impersonated (function-impersonate + #:results-guard guard))
      (check-equal? (impersonated 1 2 3) 6)
      (check-equal? (unbox result) 6))

    (test-case "argument and result guards"
      (define arg1 (box #f))
      (define arg2 (box #f))
      (define result (box #f))
      (define (argument-guard x y)
        (set-box! arg1 x)
        (set-box! arg2 y)
        (values x y))
      (define (result-guard x)
        (set-box! result x)
        x)
      (define impersonated
        (function-impersonate expt
                              #:arguments-guard argument-guard
                              #:results-guard result-guard))
      (check-equal? (impersonated 2 5) 32)
      (check-equal? (unbox arg1) 2)
      (check-equal? (unbox arg2) 5)
      (check-equal? (unbox result) 32))))
