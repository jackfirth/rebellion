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
  [enum-type? predicate/c]
  [enum-type-name (-> enum-type? interned-symbol?)]
  [enum-type-constants (-> enum-type? keyset?)]
  [enum-type-predicate-name (-> enum-type? interned-symbol?)]
  [enum-type-discriminator-name (-> enum-type? interned-symbol?)]
  [enum-type-selector-name (-> enum-type? interned-symbol?)]
  [enum-type-size (-> enum-type? natural?)]))

(require racket/math
         racket/syntax
         rebellion/base/symbol
         rebellion/collection/keyset/low-dependency
         rebellion/type/tuple)

;@------------------------------------------------------------------------------

(define-tuple-type enum-type
  (name constants predicate-name discriminator-name selector-name)
  #:omit-root-binding)

(define (enum-type name fields
                   #:predicate-name [predicate-name* #f]
                   #:discriminator-name [discriminator-name* #f]
                   #:selector-name [selector-name* #f])
  (define predicate-name (or predicate-name* (format-symbol "~a?" name)))
  (define discriminator-name
    (or discriminator-name* (format-symbol "discriminator:~a" name)))
  (define selector-name (or selector-name* (format-symbol "selector:~a" name)))
  (constructor:enum-type name
                         fields
                         predicate-name
                         discriminator-name
                         selector-name))

(define (enum-type-size type) (keyset-size (enum-type-constants type)))
