#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [atomic-boolean? predicate/c]
  [make-atomic-boolean (-> boolean? atomic-boolean?)]
  [atomic-boolean-get (-> atomic-boolean? boolean?)]
  [rename set-atomic-boolean-get! atomic-boolean-set!
          (-> atomic-boolean? boolean? void?)]
  [atomic-boolean-compare-and-set!
   (-> atomic-boolean? boolean? boolean? boolean?)]
  [atomic-boolean-compare-and-exchange!
   (-> atomic-boolean? boolean? boolean? boolean?)]
  [atomic-boolean-get-then-set! (-> atomic-boolean? boolean? boolean?)]))

(require (only-in racket/unsafe/ops unsafe-struct*-cas!)
         rebellion/base/symbol)

(module+ test
  (require (submod "..")
           rackunit
           rebellion/private/static-name))

;@------------------------------------------------------------------------------

(struct atomic-boolean ([get #:mutable])
  #:constructor-name make-atomic-boolean
  #:authentic)

(define (atomic-boolean-compare-and-set! bool expected replacement)
  (unsafe-struct*-cas! bool 0 expected replacement))

(define (atomic-boolean-compare-and-exchange! bool expected replacement)
  (if (atomic-boolean-compare-and-set! bool expected replacement)
      expected
      (not expected)))

(define (atomic-boolean-get-then-set! bool replacement)
  (atomic-boolean-compare-and-exchange! bool (not replacement) replacement))

(module+ test
  (test-case (name-string atomic-boolean-compare-and-exchange!)

    (test-case "stay false"
      (define bool (make-atomic-boolean #f))
      (check-false (atomic-boolean-compare-and-exchange! bool #t #f))
      (check-false (atomic-boolean-get bool))
      (check-false (atomic-boolean-compare-and-exchange! bool #f #f))
      (check-false (atomic-boolean-get bool)))

    (test-case "stay true"
      (define bool (make-atomic-boolean #t))
      (check-true (atomic-boolean-compare-and-exchange! bool #f #t))
      (check-true (atomic-boolean-get bool))
      (check-true (atomic-boolean-compare-and-exchange! bool #t #t))
      (check-true (atomic-boolean-get bool)))

    (test-case "set false to true"
      (define bool (make-atomic-boolean #f))
      (check-false (atomic-boolean-compare-and-exchange! bool #t #t))
      (check-false (atomic-boolean-get bool))
      (check-false (atomic-boolean-compare-and-exchange! bool #f #t))
      (check-true (atomic-boolean-get bool)))

    (test-case "set true to false"
      (define bool (make-atomic-boolean #t))
      (check-true (atomic-boolean-compare-and-exchange! bool #f #f))
      (check-true (atomic-boolean-get bool))
      (check-true (atomic-boolean-compare-and-exchange! bool #t #f))
      (check-false (atomic-boolean-get bool))))

  (test-case (name-string atomic-boolean-get-then-set!)

    (test-case "stay false"
      (define bool (make-atomic-boolean #f))
      (check-false (atomic-boolean-get-then-set! bool #f))
      (check-false (atomic-boolean-get bool)))

    (test-case "stay true"
      (define bool (make-atomic-boolean #t))
      (check-true (atomic-boolean-get-then-set! bool #t))
      (check-true (atomic-boolean-get bool)))

    (test-case "set false to true"
      (define bool (make-atomic-boolean #f))
      (check-false (atomic-boolean-get-then-set! bool #t))
      (check-true (atomic-boolean-get bool)))

    (test-case "set true to false"
      (define bool (make-atomic-boolean #t))
      (check-true (atomic-boolean-get-then-set! bool #f))
      (check-false (atomic-boolean-get bool)))))
