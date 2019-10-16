#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [entry (-> any/c any/c entry?)]
  [entry? (-> any/c boolean?)]
  [entry-key (-> entry? any/c)]
  [entry-value (-> entry? any/c)]
  [bisecting (-> (-> any/c any/c) (-> any/c any/c) transducer?)]
  [mapping-keys (-> (-> any/c any/c) transducer?)]
  [mapping-values (-> (-> any/c any/c) transducer?)]
  [indexing (-> (-> any/c any/c) transducer?)]
  [filtering-keys (-> predicate/c transducer?)]
  [filtering-values (-> predicate/c transducer?)]))

(require rebellion/collection/list
         rebellion/streaming/transducer
         rebellion/type/tuple)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define-tuple-type entry (key value))

(define (bisecting key-function value-function)
  (mapping
   (λ (element) (entry (key-function element) (value-function element)))))

(define (mapping-keys key-function)
  (mapping (λ (e) (entry (key-function (entry-key e)) (entry-value e)))))

(define (mapping-values value-function)
  (mapping (λ (e) (entry (entry-key e) (value-function (entry-value e))))))

(define (indexing key-function) (bisecting key-function values))

(define (filtering-keys key-predicate)
  (filtering (λ (e) (key-predicate (entry-key e)))))

(define (filtering-values value-predicate)
  (filtering (λ (e) (value-predicate (entry-value e)))))

(module+ test
  (test-case "bisecting"
    (check-equal? (transduce (list "the" "quick" "brown" "fox")
                             (bisecting string->symbol string-length)
                             #:into into-list)
                  (list (entry 'the 3)
                        (entry 'quick 5)
                        (entry 'brown 5)
                        (entry 'fox 3))))
  (test-case "mapping-keys"
    (check-equal? (transduce (list (entry "foo" 1) (entry "bar" 2))
                             (mapping-keys string->symbol)
                             #:into into-list)
                  (list (entry 'foo 1) (entry 'bar 2))))
  (test-case "mapping-values"
    (check-equal? (transduce (list (entry 'foo 1) (entry 'bar 2))
                             (mapping-values (λ (x) (* x 2)))
                             #:into into-list)
                  (list (entry 'foo 2) (entry 'bar 4))))
  (test-case "indexing"
    (check-equal? (transduce (list "the" "quick" "brown" "fox")
                             (indexing string-length)
                             #:into into-list)
                  (list (entry 3 "the")
                        (entry 5 "quick")
                        (entry 5 "brown")
                        (entry 3 "fox"))))
  (test-case "filtering-keys"
    (check-equal? (transduce (list (entry 1 'foo) (entry 2 'bar) (entry 3 'baz))
                             (filtering-keys even?)
                             #:into into-list)
                  (list (entry 2 'bar))))
  (test-case "filtering-values"
    (check-equal? (transduce (list (entry 'foo 1) (entry 'bar 2) (entry 'baz 3))
                             (filtering-values odd?)
                             #:into into-list)
                  (list (entry 'foo 1) (entry 'baz 3)))))
