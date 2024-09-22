#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [equivalence-relation? (-> any/c boolean?)]
  [make-equivalence-relation
   (->* ((-> any/c any/c boolean?))
        (#:name (or/c interned-symbol? #f))
        equivalence-relation?)]
  [equivalence-relation-holds? (-> equivalence-relation? any/c any/c boolean?)]
  [equivalence-relation-function
   (-> equivalence-relation? (-> any/c any/c boolean?))]
  [natural-equality equivalence-relation?]
  [object-identity-equality equivalence-relation?]
  [numeric-equality equivalence-relation?]
  [equivalence-relation-map
   (-> equivalence-relation? (-> any/c any/c) equivalence-relation?)]))

(require rebellion/base/symbol
         rebellion/private/static-name
         rebellion/type/object)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define-object-type equivalence-relation (function)
  #:constructor-name constructor:equivalence-relation)

(define (make-equivalence-relation function #:name [name #f])
  (constructor:equivalence-relation #:function function #:name name))

(define (equivalence-relation-holds? relation x y)
  ((equivalence-relation-function relation) x y))

(define/name natural-equality
  (make-equivalence-relation equal? #:name enclosing-variable-name))

(define/name object-identity-equality
  (make-equivalence-relation eq? #:name enclosing-variable-name))

(define/name numeric-equality
  (make-equivalence-relation = #:name enclosing-variable-name))

(module+ test
  (struct foo (value) #:transparent)
  (define x (foo 1))
  (define y (foo 1))
  (define z (foo 2))
  (test-case (name-string natural-equality)
    (check-true (equivalence-relation-holds? natural-equality x y))
    (check-false (equivalence-relation-holds? natural-equality x z))
    (check-false (equivalence-relation-holds? natural-equality 1 1.0))
    (check-true (equivalence-relation-holds? natural-equality +nan.0 +nan.0)))
  (test-case (name-string object-identity-equality)
    (check-true (equivalence-relation-holds? object-identity-equality x x))
    (check-false (equivalence-relation-holds? object-identity-equality x y)))
  (test-case (name-string numeric-equality)
    (check-true (equivalence-relation-holds? numeric-equality 1 1.0))
    (check-true (equivalence-relation-holds? numeric-equality 0.0 -0.0))
    (check-false (equivalence-relation-holds? numeric-equality 1 2))
    (check-false (equivalence-relation-holds? numeric-equality +nan.0 +nan.0))))

(define (equivalence-relation-map relation f)
  (define original (equivalence-relation-function relation))
  (make-equivalence-relation (λ (x y) (original (f x) (f y)))))

(module+ test
  (test-case (name-string equivalence-relation-map)
    (define rel
      (equivalence-relation-map natural-equality string-length))
    (check-true (equivalence-relation-holds? rel "foo" "bar"))
    (check-false (equivalence-relation-holds? rel "foo" "barrr"))))
