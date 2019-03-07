#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [multiset (-> any/c ... multiset?)]
  [multiset? (-> any/c boolean?)]
  [multiset-count (-> multiset? any/c natural?)]
  [multiset->hash (-> multiset? (hash/c any/c natural? #:immutable #t))]
  [multiset->list (-> multiset? list?)]))

(require racket/math)

;@------------------------------------------------------------------------------

(struct multiset (hash)
  #:transparent
  #:constructor-name constructor:multiset
  #:omit-define-syntaxes)

(define empty-hash (hash))

(define (multiset . vs)
  (constructor:multiset
   (for/fold ([h empty-hash])
             ([v (in-list vs)])
     (hash-set h v (add1 (hash-ref h v 0))))))

(define (multiset-count set elem)
  (hash-ref (multiset-hash set) elem))

(define (multiset->hash set) (multiset-hash set))

(define (multiset->list set)
  (for*/list ([(v frequency) (in-hash (multiset-hash set))]
              [_ (in-range frequency)])
    v))
