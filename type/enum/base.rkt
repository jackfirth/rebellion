#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [enum-type
   (->* (interned-symbol? keyset?)
        (#:predicate-name (or/c interned-symbol? #f)
         #:discriminator-name (or/c interned-symbol? #f)
         #:selector-name (or/c interned-symbol? #f))
        enum-type?)]
  [enum-type? (-> any/c boolean?)]
  [enum-type-name (-> enum-type? interned-symbol?)]
  [enum-type-constants (-> enum-type? keyset?)]
  [enum-type-predicate-name (-> enum-type? interned-symbol?)]
  [enum-type-discriminator-name (-> enum-type? interned-symbol?)]
  [enum-type-selector-name (-> enum-type? interned-symbol?)]
  [enum-type-size (-> enum-type? natural?)]))

(require racket/math
         rebellion/base/symbol
         rebellion/collection/keyset/low-dependency
         rebellion/type/private/naming
         rebellion/type/tuple)

;@------------------------------------------------------------------------------

(define-tuple-type enum-type
  (name constants predicate-name discriminator-name selector-name)
  #:omit-root-binding)

(define (enum-type name constants
                   #:predicate-name [predicate-name* #f]
                   #:discriminator-name [discriminator-name* #f]
                   #:selector-name [selector-name* #f])
  (define predicate-name (or predicate-name* (default-predicate-name name)))
  (define discriminator-name
    (or discriminator-name* (default-discriminator-name name)))
  (define selector-name (or selector-name* (default-selector-name name)))
  (constructor:enum-type name
                         constants
                         predicate-name
                         discriminator-name
                         selector-name))

(define (enum-type-size type) (keyset-size (enum-type-constants type)))
