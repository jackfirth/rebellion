#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [subsequence
   (->i ([sequence sequence?]
         [start natural?])
        ([end (start) (or/c (integer-in start #f) #f)])
        [_ sequence?])]))

(require racket/math
         racket/sequence
         rebellion/private/guarded-block)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define/guard (subsequence sequence start [end #false])
  (guard end else (sequence-tail sequence start))
  (define limit (- end start))
  (define indexed-tail
    (sequence-map cons (in-indexed (sequence-tail sequence start))))
  (define indexed-subsequence
    (stop-before indexed-tail (λ (pair) (>= (cdr pair) limit))))
  (sequence-map car indexed-subsequence))

(module+ test
  (test-case "subsequence"
    (test-case "no end"
      (define seq (subsequence "hello world" 3))
      (check-equal? (list->string (sequence->list seq)) "lo world"))

    (test-case "end"
      (define seq (subsequence "hello world" 3 8))
      (check-equal? (list->string (sequence->list seq)) "lo wo"))))
