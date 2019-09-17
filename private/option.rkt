#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [option? predicate/c]
  [option-case
   (-> option? #:present (-> any/c any/c) #:absent (-> any/c) any/c)]
  [option-map (-> option? (-> any/c any/c) option?)]
  [option-filter (-> option? predicate/c option?)]
  [option-flat-map (-> option? (-> any/c option?) option?)]
  [option-get (-> option? any/c any/c)]
  [present (-> any/c present?)]
  [present? predicate/c]
  [present-value (-> present? any/c)]
  [absent absent?]
  [absent? predicate/c]
  [option/c (-> chaperone-contract? chaperone-contract?)]
  [present/c (-> chaperone-contract? chaperone-contract?)]))

(require racket/contract/combinator
         rebellion/private/contract-projection
         rebellion/type/singleton
         rebellion/type/wrapper)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define-wrapper-type present)
(define-singleton-type absent)

(define (option? v) (or (absent? v) (present? v)))

(define (option-case opt #:present present-handler #:absent absent-handler)
  (if (present? opt) (present-handler (present-value opt)) (absent-handler)))

(define (option-map opt f)
  (if (present? opt) (present (f (present-value opt))) absent))

(define (option-filter opt pred)
  (if (and (present? opt) (pred (present-value opt))) opt absent))

(define (option-flat-map opt f)
  (if (present? opt) (f (present-value opt)) absent))

(define (option-get opt default)
  (if (present? opt) (present-value opt) default))

(module+ test
  (test-case "option-map"
    (check-equal? (option-map (present 2) add1) (present 3))
    (check-equal? (option-map absent add1) absent))
  (test-case "option-filter"
    (check-equal? (option-filter (present 2) number?) (present 2))
    (check-equal? (option-filter (present 2) string?) absent)
    (check-equal? (option-filter absent number?) absent))
  (test-case "option-flat-map"
    (define (halve x) (if (even? x) (present (/ x 2)) absent))
    (check-equal? (option-flat-map (present 4) halve) (present 2))
    (check-equal? (option-flat-map (present 5) halve) absent)
    (check-equal? (option-flat-map absent halve) absent))
  (test-case "option-get"
    (check-equal? (option-get (present 2) 100) 2)
    (check-equal? (option-get absent 100) 100))
  (test-case "option-case"
    (struct exn:kaboom exn:fail ())
    (define (kaboom) (raise (exn:kaboom "kaboom" (current-continuation-marks))))
    (check-equal? (option-case (present 2) #:present add1 #:absent kaboom) 3)
    (check-exn exn:kaboom?
               (λ () (option-case absent #:present add1 #:absent kaboom)))))

(define (option/c contract*)
  (define contract (coerce-contract 'option/c contract*))
  (define name (build-compound-type-name 'option/c contract))
  (define (late-neg blame)
    (define present-projection (contract-present-projection contract blame))
    (projection-and (contract-get-projection option? blame)
                    (projection-filter present-projection present?)))
  (define maker
    (if (flat-contract? contract) make-flat-contract make-chaperone-contract))
  (maker #:name name #:late-neg-projection late-neg))

(define (present/c contract*)
  (define contract (coerce-contract 'present/c contract*))
  (define name (build-compound-type-name 'present/c contract))
  (define (late-neg blame)
    (projection-and (contract-get-projection present? blame)
                    (contract-present-projection contract blame)))
  (define maker
    (if (flat-contract? contract) make-flat-contract make-chaperone-contract))
  (maker #:name name #:late-neg-projection late-neg))

(define (contract-present-projection contract blame)
  (define underlying-projection
    (contract-get-projection contract
                             (blame-add-context blame "the present option in")))
  (projection-convert underlying-projection present-value present))
