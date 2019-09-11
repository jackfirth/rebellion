#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [singleton-type
   (->* (interned-symbol?) (#:predicate-name (or/c interned-symbol? #f))
        singleton-type?)]
  [singleton-type? predicate/c]
  [singleton-type-name (-> singleton-type? interned-symbol?)]
  [singleton-type-predicate-name (-> singleton-type? interned-symbol?)]))

(require rebellion/base/symbol
         rebellion/type/tuple)

;@------------------------------------------------------------------------------

(define-tuple-type singleton-type (name predicate-name)
  #:constructor-name constructor:singleton-type)

(define (singleton-type name #:predicate-name [pred-name #f])
  (constructor:singleton-type name pred-name))
