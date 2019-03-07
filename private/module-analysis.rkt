#lang racket

(require racket/pretty
         rebellion/entry
         rebellion/module-export
         rebellion/multiset)

;@------------------------------------------------------------------------------

(define empty-hash (hash))

(define (symbol-first-word sym)
  (define words (string-split (symbol->string sym) "-"))
  (and (not (empty? words)) (first words)))

(define (symbol-words sym)
  (map string->symbol (string-split (symbol->string sym) "-")))

(define (group-by/hash f vs)
  (for/fold ([h empty-hash])
            ([v (in-list vs)])
    (define k (f v))
    (hash-set h k (cons v (hash-ref h k empty)))))

(define (hash-map/into-hash h f)
  (for/hash ([(k v) (in-hash h)])
    (values k (f v))))

(define (hash->entries h) (hash-map h entry))

;@------------------------------------------------------------------------------

(define (top-k-most-common-words-in-exported-names modname k)
  (take (sort #:key entry-value
              (hash->entries
               (multiset->hash
                (apply multiset
                       (append-map symbol-words
                                   (map export-name
                                        (set->list
                                         (module-exports modname)))))))
              >)
        k))

(module+ main
  (define entries (top-k-most-common-words-in-exported-names 'racket/base 10))
  (for ([e (in-list entries)])
    (write-string (~a (~a (entry-key e) #:min-width 10) (entry-value e)))
    (newline)))
