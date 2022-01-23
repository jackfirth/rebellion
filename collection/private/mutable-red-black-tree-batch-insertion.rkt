#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [mutable-rb-tree-put-all! (-> mutable-rb-tree? (sequence/c entry?) #:who interned-symbol? void?)]))


(require racket/match
         racket/sequence
         rebellion/base/option
         rebellion/base/symbol
         rebellion/collection/entry
         rebellion/collection/private/mutable-red-black-tree-base
         rebellion/collection/private/mutable-red-black-tree-insertion
         rebellion/collection/private/mutable-red-black-tree-iteration
         rebellion/collection/private/mutable-red-black-tree-search)


;@----------------------------------------------------------------------------------------------------


(define (mutable-rb-tree-put-all! tree entries #:who who)
  (define unique-entries (make-mutable-rb-tree (mutable-rb-tree-key-comparator tree)))
  (for ([e entries])
    (match-define (entry key value) e)
    (match (mutable-rb-tree-get-option unique-entries key)
      [(present first-value)
       (raise-arguments-error
        who
        "cannot batch insert entries, entry batch contain duplicate keys"
        "key" key
        "first value" first-value
        "duplicate value" value)]
      [(== absent)
       (mutable-rb-tree-put! unique-entries key value)]))
  ;; This could be much faster since we're combining two red-black trees and there exist efficient
  ;; algorithms for that. But that's complicated and the naive approach is obviously correct. Future
  ;; work may optimize this but the simple and correct approach is fine for now.
  (for ([e (in-mutable-rb-tree unique-entries)])
    (mutable-rb-tree-put! tree (entry-key e) (entry-value e))))
