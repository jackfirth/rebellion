#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [weird-hill-climbing-tree-search
   (-> any/c
       #:accept-when predicate/c
       #:reject-when predicate/c
       #:search-using (-> any/c list?)
       list?)]))

(require racket/list)

(module+ test
  (require rackunit
           racket/set))

;@------------------------------------------------------------------------------

;; TODO(https://github.com/jackfirth/rebellion/issues/81)
(define empty-list empty)
(define empty-list? empty?)

;; TODO: find a better name
(define (weird-hill-climbing-tree-search
         initial-state
         #:accept-when good-enough-state?
         #:reject-when invalid-state?
         #:search-using local-search-function)
  (let loop ([candidate-states (list initial-state)]
             [final-states empty-list])
    (cond
      [(empty-list? candidate-states) (reverse final-states)]
      [else
       (define candidate (first candidate-states))
       (define remaining-candidates (rest candidate-states))
       (cond
         [(invalid-state? candidate)
          (loop remaining-candidates final-states)]
         [(good-enough-state? candidate)
          (loop remaining-candidates (cons candidate final-states))]
         [else
          (define successors (local-search-function candidate))
          (loop (append successors remaining-candidates) final-states)])])))

(module+ test
  (test-case "weird-hill-climbing-tree-search"
    (define-syntax-rule (list-when [condition expr ...] ...)
      (append (if condition (list expr ...) empty-list) ...))

    (struct triple (x y z) #:transparent)

    (define (nonpositive-triple? t)
      (define x (triple-x t))
      (define y (triple-y t))
      (define z (triple-z t))
      (not (and (positive? x) (positive? y) (positive? z))))

    (define (small? x) (< x 6))
    (define (small-sorted-triple? t)
      (define x (triple-x t))
      (define y (triple-y t))
      (define z (triple-z t))
      (and (small? x) (small? y) (small? z) (< x y z)))

    (define (find-better-triples s)
      (define x (triple-x s))
      (define y (triple-y s))
      (define z (triple-z s))
      (define already-sorted? (< x y z))
      (list-when
       [(< y x) (triple y x z)]
       [(< z y) (triple x z y)]
       [(and already-sorted? (even? z)) (triple x y (/ z 2))]
       [(and already-sorted? (odd? z))
        (triple (sub1 x) y z)
        (triple x (sub1 y) z)
        (triple x y (sub1 z))]))

    (define all-small-sorted-triples
      (set (triple 1 2 3)
           (triple 1 2 4)
           (triple 1 2 5)
           (triple 1 3 4)
           (triple 1 3 5)
           (triple 1 4 5)
           (triple 2 3 4)
           (triple 2 3 5)
           (triple 2 4 5)
           (triple 3 4 5)))
    
    (check-equal? (list->set
                   (weird-hill-climbing-tree-search
                    (triple 11 24 18)
                    #:accept-when small-sorted-triple?
                    #:reject-when nonpositive-triple?
                    #:search-using find-better-triples))
                  all-small-sorted-triples)))
