#lang parendown racket/base

(require racket/contract/base)

(provide #/contract-out
  [empty-list empty-list?]
  [empty-list? predicate/c]
  [nonempty-list? predicate/c]
  [list-insert (-> list? any/c nonempty-list?)]
  [list-first (-> nonempty-list? any/c)]
  [list-rest (-> nonempty-list? list?)]
  [list-append (-> list? ... list?)]
  [list-size (-> list? natural?)]
  [list-ref-safe (-> list? natural? option?)]
  [list-contains? (-> list? any/c boolean?)]
  [list-reverse (-> list? list?)]
  [into-list reducer?]
  [into-reversed-list reducer?]
  [appending-into-list reducer?])

(require racket/math
         (only-in lathe-comforts expect fn w-loop)
         (only-in lathe-comforts/list nat->maybe)
         (only-in lathe-comforts/maybe just)
         rebellion/base/option
         rebellion/streaming/reducer)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define empty-list (list))
(define (empty-list? v) (and (list? v) (equal? v empty-list)))
(define (nonempty-list? v) (and (list? v) (not #/equal? v empty-list)))

(define (list-size lst) (length lst))
(define (list-insert lst v) (cons v lst))
(define (list-first lst) (car lst))
(define (list-rest lst) (cdr lst))
(define (list-append . lsts) (apply append lsts))
(define (list-reverse lst) (reverse lst))
(define (list-contains? lst v) (not #/not #/member v lst))

(define (list-ref-safe lst pos)
  (w-loop loop lst lst pos pos
    (expect lst (cons elem lst) absent
    #/expect (nat->maybe pos) (just pos) (present elem)
    #/loop lst pos)))

(define into-list
  (make-effectful-fold-reducer list-insert
                               (fn empty-list)
                               list-reverse
                               #:name 'into-list))

(define into-reversed-list
  (make-fold-reducer list-insert empty-list #:name 'into-reversed-list))

(define appending-into-list
  (reducer-map into-list #:range #/fn lst #/apply list-append lst))

(module+ test
  (test-case "list-ref-safe"
    (define lst (list 'a 'b 'c))
    (check-equal? (list-ref-safe lst 0) (present 'a))
    (check-equal? (list-ref-safe lst 2) (present 'c))
    (check-equal? (list-ref-safe lst 3) absent))
  (test-case "list-contains?"
    (check-true #/list-contains? (list 1 2 3) 1)
    (check-true #/list-contains? (list 1 2 3) 2)
    (check-true #/list-contains? (list 1 2 3) 3)
    (check-false #/list-contains? (list 1 2 3) 4))
  (test-case "empty-lists"
    (check-pred empty-list? empty-list)
    (check-pred empty-list? (list))
    (check-pred nonempty-list? (list 1 2 3))
    (check-false #/empty-list? #/list 1 2 3)
    (check-false #/nonempty-list? empty-list)
    (check-false #/nonempty-list? #/list))
  (test-case "into-list"
    (check-equal? (reduce into-list 1 2 3 4 5) (list 1 2 3 4 5))
    (check-equal? (reduce into-list) empty-list))
  (test-case "into-reversed-list"
    (check-equal? (reduce into-reversed-list 1 2 3 4 5) (list 5 4 3 2 1))
    (check-equal? (reduce into-reversed-list) empty-list))
  (test-case "appending-into-list"
    (check-equal? (reduce appending-into-list
                          (list 1 2 3)
                          (list 'a 'b)
                          empty-list
                          (list 'foo)
                          (list 4 5 6 7 8))
                  (list 1 2 3 'a 'b 'foo 4 5 6 7 8))))
