#lang racket/base


(require racket/contract/base)


(provide present)


(provide
 (contract-out
  [option? (-> any/c boolean?)]
  [option-case (-> option? #:present (-> any/c any/c) #:absent (-> any/c) any/c)]
  [option-map (-> option? (-> any/c any/c) option?)]
  [option-filter (-> option? (-> any/c boolean?) option?)]
  [option-flat-map (-> option? (-> any/c option?) option?)]
  [option-or (-> option? option? ... option?)]
  [option-or-call (-> option? (-> option?) ... option?)]
  [option-get (-> option? any/c any/c)]
  [falsey->option (-> any/c option?)]
  [in-option (-> option? (sequence/c any/c))]
  [present? (-> any/c boolean?)]
  [present-value (-> present? any/c)]
  [absent absent?]
  [absent? (-> any/c boolean?)]
  [option/c (-> chaperone-contract? chaperone-contract?)]
  [present/c (-> chaperone-contract? chaperone-contract?)]))


(require racket/contract/combinator
         racket/list
         racket/match
         racket/sequence
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
  (match opt [(present v) (present-handler v)] [_ (absent-handler)]))


(define (option-map opt f)
  (match opt [(present v) (present (f v))] [_ absent]))


(define (option-filter opt pred)
  (match opt [(present (? pred)) opt] [_ absent]))


(define (option-flat-map opt f)
  (match opt [(present v) (f v)] [_ absent]))


(define (option-or first-opt . opts)
  (let loop ([first-opt first-opt] [opts opts])
    (cond
      [(present? first-opt) first-opt]
      [(empty? opts) absent]
      [else (loop (first opts) (rest opts))])))


(define (option-or-call first-opt . opt-thunks)
  (let loop ([first-opt first-opt] [opt-thunks opt-thunks])
    (cond
      [(present? first-opt) first-opt]
      [(empty? opt-thunks) absent]
      [else (loop ((first opt-thunks)) (rest opt-thunks))])))


(define (option-get opt default)
  (match opt [(present v) v] [_ default]))


(define (falsey->option v)
  (if v (present v) absent))


(define (in-option opt)
  (match opt [(present v) (list v)] [_ '()]))


(module+ test
  (test-case "pattern matching"
    (check-match (present 42) (present _))
    (check-match absent (== absent)))
  
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

  (test-case "option-or"
    (check-equal? (option-or absent) absent)
    (check-equal? (option-or (present 1)) (present 1))
    (check-equal? (option-or (present 1) (present 2)) (present 1))
    (check-equal? (option-or (present 1) absent) (present 1))
    (check-equal? (option-or absent (present 1)) (present 1))
    (check-equal? (option-or absent absent) absent))

  (test-case "option-or-call"
    (check-equal? (option-or-call absent) absent)
    (check-equal? (option-or-call (present 1)) (present 1))
    (check-equal? (option-or-call (present 1) (λ () (present 2))) (present 1))
    (check-equal? (option-or-call (present 1) (λ () absent)) (present 1))
    (check-equal? (option-or-call absent (λ () (present 1))) (present 1))
    (check-equal? (option-or-call absent (λ () absent)) absent))

  (test-case "option-get"
    (check-equal? (option-get (present 2) 100) 2)
    (check-equal? (option-get absent 100) 100))

  (test-case "falsey->option"
    (check-equal? (falsey->option 5) (present 5))
    (check-equal? (falsey->option #false) absent))

  (test-case "option-case"
    (struct exn:kaboom exn:fail ())
    (define (kaboom) (raise (exn:kaboom "kaboom" (current-continuation-marks))))
    (check-equal? (option-case (present 2) #:present add1 #:absent kaboom) 3)
    (check-exn exn:kaboom?
               (λ () (option-case absent #:present add1 #:absent kaboom))))

  (test-case "in-option"
    (check-equal? (for/list ([v (in-option (present 1))]) v) (list 1))
    (check-equal? (for/list ([v (in-option absent)]) v) '())))


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
