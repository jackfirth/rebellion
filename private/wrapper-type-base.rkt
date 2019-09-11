#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [wrapper-type
   (->* (interned-symbol?)
        (#:predicate-name (or/c interned-symbol? #f)
         #:constructor-name (or/c interned-symbol? #f)
         #:accessor-name (or/c interned-symbol? #f))
        wrapper-type?)]
  [wrapper-type? predicate/c]
  [wrapper-type-name (-> wrapper-type? interned-symbol?)]
  [wrapper-type-constructor-name (-> wrapper-type? interned-symbol?)]
  [wrapper-type-predicate-name (-> wrapper-type? interned-symbol?)]
  [wrapper-type-accessor-name (-> wrapper-type? interned-symbol?)]))

(require racket/syntax
         rebellion/base/symbol
         rebellion/type/record)

;@------------------------------------------------------------------------------

(define-record-type wrapper-type
  (name predicate-name constructor-name accessor-name)
  #:constructor-name constructor:wrapper-type)

(define (wrapper-type
         name
         #:predicate-name [predicate-name #f]
         #:constructor-name [constructor-name #f]
         #:accessor-name [accessor-name #f])
  (constructor:wrapper-type
   #:name name
   #:predicate-name (or predicate-name (format-symbol "~a?" name))
   #:constructor-name (or constructor-name name)
   #:accessor-name (or accessor-name (format-symbol "~a-value" name))))
