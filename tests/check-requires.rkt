#lang racket/base

(module+ test
  (require macro-debugger/analysis/check-requires
           racket/port
           racket/set
           racket/string)

  (define modules
    (set 'rebellion
         'rebellion/boolean
         'rebellion/custom-write
         'rebellion/custom-write/struct
         'rebellion/custom-write/tuple
         'rebellion/entry
         'rebellion/equal+hash
         'rebellion/equal+hash/struct
         'rebellion/equal+hash/tuple
         'rebellion/filter
         'rebellion/generative-token
         'rebellion/info
         'rebellion/keyset
         'rebellion/keyword
         'rebellion/main
         'rebellion/module-export
         'rebellion/name
         'rebellion/natural
         'rebellion/pair
         'rebellion/permutation
         'rebellion/point
         'rebellion/predicate
         'rebellion/private/boolean
         'rebellion/private/custom-write-struct
         'rebellion/private/custom-write-tuple
         'rebellion/private/custom-write
         'rebellion/private/entry
         'rebellion/private/equal+hash-struct
         'rebellion/private/equal+hash-tuple
         'rebellion/private/equal+hash
         'rebellion/private/filter
         'rebellion/private/generative-token
         'rebellion/private/keyset
         'rebellion/private/keyword
         'rebellion/private/module-export
         'rebellion/private/name
         'rebellion/private/natural
         'rebellion/private/pair
         'rebellion/private/permutation
         'rebellion/private/point
         'rebellion/private/predicate
         'rebellion/private/probability-distribution
         'rebellion/private/record-field
         'rebellion/private/record
         'rebellion/private/scribble-evaluator-factory
         'rebellion/private/singleton
         'rebellion/private/struct-definition-util
         'rebellion/private/struct-descriptor
         'rebellion/private/symbol
         'rebellion/private/table
         'rebellion/private/tuple-type-definition
         'rebellion/private/tuple-type
         'rebellion/private/variant
         'rebellion/private/web-graph
         'rebellion/private/web-link
         'rebellion/record
         'rebellion/record/field
         'rebellion/singleton
         'rebellion/struct-descriptor
         'rebellion/symbol
         'rebellion/table
         'rebellion/tests/check-requires
         'rebellion/tuple-type-definition
         'rebellion/tuple-type
         'rebellion/variant
         'rebellion/web-graph
         'rebellion/web-link))
  
  (for ([mod (in-set modules)])
    (define output
      (with-output-to-string
        (λ ()
          (check-requires mod
                          #:show-keep? #f
                          #:show-bypass? #t
                          #:show-drop? #t
                          #:show-uses? #f))))
    (when (non-empty-string? output)
      (parameterize ([current-output-port (current-error-port)])
        (printf "==== ~a ====\n" mod)
        (write-string output)))))
