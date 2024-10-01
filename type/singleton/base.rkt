#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [singleton-type
   (->* (interned-symbol?) (#:predicate-name (or/c interned-symbol? #f))
        singleton-type?)]
  [singleton-type? (-> any/c boolean?)]
  [singleton-type-name (-> singleton-type? interned-symbol?)]
  [singleton-type-predicate-name (-> singleton-type? interned-symbol?)]))

(require rebellion/base/symbol
         rebellion/type/private/naming
         rebellion/type/tuple)

;@------------------------------------------------------------------------------

(define-tuple-type singleton-type (name predicate-name) #:omit-root-binding)

(define (singleton-type name #:predicate-name [predicate-name* #f])
  (define predicate-name (or predicate-name* (default-predicate-name name)))
  (constructor:singleton-type name predicate-name))
