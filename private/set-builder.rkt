#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [set-builder? predicate/c]
  [unused-set-builder/c flat-contract?]
  [make-set-builder (->* () ((sequence/c any/c)) unused-set-builder/c)]
  [set-builder-add (-> unused-set-builder/c any/c ... unused-set-builder/c)]
  [set-builder-add-all
   (-> unused-set-builder/c (sequence/c any/c) unused-set-builder/c)]
  [build-set (-> unused-set-builder/c set?)]
  [build-mutable-set (-> unused-set-builder/c set-mutable?)]))

(require racket/contract/combinator
         racket/sequence
         racket/set
         rebellion/collection/list
         rebellion/private/static-name)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(struct set-builder
  ([uses-remaining #:mutable]
   next-total-uses
   backing-mutable-set)
  #:constructor-name constructor:set-builder)

(define/name unused-set-builder/c
  (flat-contract-with-explanation
   (λ (v)
     (cond
       [(not (set-builder? v))
        (define template '(expected: "set-builder?" given: "~e"))
        (λ (blame) (raise-blame-error blame v template v))]
       [(not (positive? (set-builder-uses-remaining v)))
        (define template
          '("this builder has already been used\n  builder: ~e"))
        (λ (blame) (raise-blame-error blame v template v))]
       [else #t]))
   #:name enclosing-variable-name))

(define (make-set-builder [initial-contents empty-list])
  (define backing-mutable-set (mutable-set))
  (for ([v initial-contents]) (set-add! backing-mutable-set v))
  (constructor:set-builder 1 2 backing-mutable-set))

(define (set-builder-mark-used-once builder)
  (define new-uses (sub1 (set-builder-uses-remaining builder)))
  (set-set-builder-uses-remaining! builder new-uses)
  (cond
    [(zero? new-uses)
     (define next-total (set-builder-next-total-uses builder))
     (define backing-mutable-set (set-builder-backing-mutable-set builder))
     (constructor:set-builder next-total (* next-total 2) backing-mutable-set)]
    [else builder]))

(define (set-builder-add builder . vs)
  (define backing-mutable-set (set-builder-backing-mutable-set builder))
  (for ([v (in-list vs)]) (set-add! backing-mutable-set v))
  (set-builder-mark-used-once builder))

(define (set-builder-add-all builder seq)
  (define backing-mutable-set (set-builder-backing-mutable-set builder))
  (for ([v seq]) (set-add! backing-mutable-set v))
  (set-builder-mark-used-once builder))

(define (build-set builder)
  (set-set-builder-uses-remaining! builder 0)
  (set-union (set) (set-builder-backing-mutable-set builder)))

(define (build-mutable-set builder)
  (set-set-builder-uses-remaining! builder 0)
  (set-builder-backing-mutable-set builder))

(module+ test
  (test-case "set builders should support basic folding"
    (define st
      (build-set
       (foldl (λ (v builder) (set-builder-add builder v))
              (make-set-builder)
              (list 1 2 3 1 3))))
    (check-true (set? st))
    (check-equal? st (set 1 2 3)))

  (test-case "set builders should raise errors when used after building"
    (define builder
      (foldl (λ (v builder) (set-builder-add builder v))
             (make-set-builder)
             (list 1 2 3 1 3)))
    (define st (build-set builder))
    (check-exn #rx"this builder has already been used"
               (λ () (build-set builder)))
    (check-exn #rx"this builder has already been used"
               (λ () (build-set builder)))
    (check-exn #rx"this builder has already been used"
               (λ () (set-builder-add builder 1)))
    (check-exn #rx"this builder has already been used"
               (λ () (set-builder-add-all builder (list 1 2 3))))
    (check-exn #rx"this builder has already been used"
               (λ () (build-mutable-set builder))))

  (test-case "set-builder-add"
    (define builder0 (make-set-builder))
    (define builder1 (set-builder-add builder0 1 2 3))
    (define builder2 (set-builder-add builder1 1 4 5))
    (define st (build-set builder2))
    (check-equal? st (set 1 2 3 4 5)))

  (test-case "set-builder-add-all"
    (define builder0 (make-set-builder))
    (define builder1 (set-builder-add-all builder0 (in-range 0 7)))
    (define builder2 (set-builder-add-all builder1 (in-range 4 10)))
    (define st (build-set builder2))
    (check-equal? st (set 0 1 2 3 4 5 6 7 8 9)))

  (test-case "set builders should not allow multiple usages"
    (define builder (make-set-builder))
    (set-builder-add builder 42)
    (check-exn #rx"this builder has already been used"
               (λ () (set-builder-add builder 42)))))
