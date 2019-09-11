#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [record-type (->* (interned-symbol? keyset?)
                    (#:predicate-name (or/c interned-symbol? #f)
                     #:constructor-name (or/c interned-symbol? #f)
                     #:accessor-name (or/c interned-symbol? #f))
                    record-type?)]
  [record-type? predicate/c]
  [record-type-name (-> record-type? interned-symbol?)]
  [record-type-fields (-> record-type? keyset?)]
  [record-type-predicate-name (-> record-type? interned-symbol?)]
  [record-type-constructor-name (-> record-type? interned-symbol?)]
  [record-type-accessor-name (-> record-type? interned-symbol?)]))

(require racket/syntax
         rebellion/base/symbol
         rebellion/collection/keyset/low-dependency
         rebellion/type/tuple)

;@------------------------------------------------------------------------------

(define-tuple-type record-type
  (name fields predicate-name constructor-name accessor-name)
  #:constructor-name constructor:record-type)

(define (record-type name fields
                     #:predicate-name [predicate-name* #f]
                     #:constructor-name [constructor-name* #f]
                     #:accessor-name [accessor-name* #f])
  (define predicate-name (or predicate-name* (format-symbol "~a?" name)))
  (define constructor-name (or constructor-name* name))
  (define accessor-name (or accessor-name* (format-symbol "~a-ref" name)))
  (constructor:record-type name
                           fields
                           predicate-name
                           constructor-name
                           accessor-name))
