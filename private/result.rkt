#lang racket/base

(require racket/contract/base)

(provide result
         success
         failure)

(provide
 (contract-out
  [result? predicate/c]
  [result-case
   (-> result? #:success (-> any/c any/c) #:failure (-> any/c any/c) any/c)]
  [result/c (-> chaperone-contract? chaperone-contract? chaperone-contract?)]
  [success? predicate/c]
  [success-value (-> success? any/c)]
  [success/c (-> chaperone-contract? chaperone-contract?)]
  [failure? predicate/c]
  [failure-error (-> failure? any/c)]
  [failure/c (-> chaperone-contract? chaperone-contract?)]))

(require racket/contract/combinator
         racket/match
         rebellion/private/contract-projection
         rebellion/private/static-name
         rebellion/type/tuple
         syntax/parse/define)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define-tuple-type success (value))
(define-tuple-type failure (error))

(define (result? v) (or (success? v) (failure? v)))

(define-simple-macro (result body:expr ...+) (call/result (λ () body ...)))

(define (call/result thunk)
  (with-handlers ([(λ (_) #t) failure]) (success (thunk))))

(define (result-case result #:success success-handler #:failure failure-handler)
  (match result
    [(success v) (success-handler v)]
    [(failure e) (failure-handler e)]))

;@------------------------------------------------------------------------------
;; Contract combinators

(define (result/c success-contract* failure-contract*)
  (define success-contract (coerce-contract 'result/c success-contract*))
  (define failure-contract (coerce-contract 'result/c failure-contract*))
  (define name
    (build-compound-type-name 'result/c success-contract failure-contract))
  (define (late-neg blame)
    (define success-projection
      (contract-success-projection success-contract blame))
    (define failure-projection
      (contract-failure-projection failure-contract blame))
    (projection-and (contract-get-projection result? blame)
                    (projection-filter success-projection success?)
                    (projection-filter failure-projection failure?)))
  (define maker
    (if (and (flat-contract? success-contract)
             (flat-contract? failure-contract))
        make-flat-contract
        make-chaperone-contract))
  (maker #:name name #:late-neg-projection late-neg))

(define (success/c contract*)
  (define contract (coerce-contract 'success/c contract*))
  (define name (build-compound-type-name 'success/c contract))
  (define (late-neg blame)
    (projection-and (contract-get-projection success? blame)
                    (contract-success-projection contract blame)))
  (define maker
    (if (flat-contract? contract) make-flat-contract make-chaperone-contract))
  (maker #:name name #:late-neg-projection late-neg))

(define (failure/c contract*)
  (define contract (coerce-contract 'failure/c contract*))
  (define name (build-compound-type-name 'failure/c contract))
  (define (late-neg blame)
    (projection-and (contract-get-projection failure? blame)
                    (contract-failure-projection contract blame)))
  (define maker
    (if (flat-contract? contract) make-flat-contract make-chaperone-contract))
  (maker #:name name #:late-neg-projection late-neg))

(define (contract-success-projection contract blame*)
  (define blame (blame-add-context blame* "the successful result in"))
  (define underlying-projection (contract-get-projection contract blame))
  (projection-convert underlying-projection success-value success))

(define (contract-failure-projection contract blame*)
  (define blame (blame-add-context blame* "the failed result in"))
  (define underlying-projection (contract-get-projection contract blame))
  (projection-convert underlying-projection failure-error failure))

(module+ test
  (test-case (name-string result)
    (check-equal? (result (+ 1 2)) (success 3))
    (check-equal? (result (define foo 1) (define bar 2) (+ foo bar))
                  (success 3))
    (check-equal? (result (raise "kaboom!")) (failure "kaboom!")))
  
  (test-case (name-string result/c)
    (check-not-exn
     (λ () (invariant-assertion (result/c number? string?) (success 42))))
    (check-not-exn
     (λ ()
       (invariant-assertion (result/c number? string?) (failure "foo"))))
    (check-exn exn:fail:contract:blame?
               (λ ()
                 (invariant-assertion (result/c number? string?) "foo")))
    (check-exn exn:fail:contract:blame?
               (λ () (invariant-assertion (result/c number? string?) 42)))
    (check-exn exn:fail:contract:blame?
               (λ ()
                 (invariant-assertion (result/c number? string?) "foo")))
    (check-exn exn:fail:contract:blame?
               (λ ()
                 (invariant-assertion (result/c number? string?)
                                      (success "foo"))))
    (check-exn exn:fail:contract:blame?
               (λ ()
                 (invariant-assertion (result/c number? string?)
                                      (failure 42)))))
  (test-case (name-string success/c)
    (check-not-exn
     (λ () (invariant-assertion (success/c number?) (success 42))))
    (check-exn exn:fail:contract:blame?
               (λ () (invariant-assertion (success/c number?) (success "foo"))))
    (check-exn exn:fail:contract:blame?
               (λ () (invariant-assertion (success/c number?) (failure 42))))
    (check-exn exn:fail:contract:blame?
               (λ () (invariant-assertion (success/c number?) 42))))
  (test-case (name-string failure/c)
    (check-not-exn
     (λ () (invariant-assertion (failure/c string?) (failure "foo"))))
    (check-exn exn:fail:contract:blame?
               (λ () (invariant-assertion (failure/c string?) (success "foo"))))
    (check-exn exn:fail:contract:blame?
               (λ () (invariant-assertion (failure/c string?) (failure 42))))
    (check-exn exn:fail:contract:blame?
               (λ () (invariant-assertion (failure/c string?) "foo")))))
