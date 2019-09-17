#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [projection/c contract?]
  [contract-get-projection (-> predicate/c blame? projection/c)]
  [projection-and (-> projection/c ... projection/c)]
  [projection-convert
   (-> projection/c (-> any/c any/c) (-> any/c any/c) projection/c)]
  [projection-filter (-> projection/c predicate/c projection/c)]))

(require racket/contract/combinator)

;@------------------------------------------------------------------------------

(define projection/c (-> any/c any/c any/c))

(define (contract-get-projection predicate blame)
  ((contract-late-neg-projection predicate) blame))
  
(define ((projection-and . projections) v missing-party)
  (for/fold ([v v]) ([p (in-list projections)])
    (p v missing-party)))

(define ((projection-convert projection forwards backwards) v missing-party)
  (backwards (projection (forwards v) missing-party)))

(define ((projection-filter projection pred) v missing-party)
  (if (pred v) (projection v missing-party) v))
