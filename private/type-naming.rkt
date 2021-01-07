#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [default-descriptor-identifier (-> identifier? identifier?)]
  [default-predicate-name (-> interned-symbol? interned-symbol?)]
  [default-predicate-identifier (-> identifier? identifier?)]
  [default-constructor-name (-> interned-symbol? interned-symbol?)]
  [default-constructor-identifier (-> identifier? identifier?)]
  [default-accessor-name (-> interned-symbol? interned-symbol?)]
  [default-accessor-identifier (-> identifier? identifier?)]
  [default-discriminator-name (-> interned-symbol? interned-symbol?)]
  [default-discriminator-identifier (-> identifier? identifier?)]
  [default-selector-name (-> interned-symbol? interned-symbol?)]
  [default-selector-identifier (-> identifier? identifier?)]
  [default-opaque-constructor-name (-> interned-symbol? interned-symbol?)]
  [default-opaque-constructor-identifier (-> identifier? identifier?)]
  [default-pattern-identifier (-> identifier? identifier?)]
  [default-setter-identifier (-> identifier? identifier?)]
  [default-field-accessor-identifier (-> identifier? identifier? identifier?)]
  [default-unwrapping-accessor-name (-> interned-symbol? interned-symbol?)]
  [default-unwrapping-accessor-identifier (-> identifier? identifier?)]
  [default-instance-identifier (-> identifier? identifier?)]
  [default-renamer-identifier (-> identifier? identifier?)]
  [default-renamer-name (-> interned-symbol? interned-symbol?)]))

(require racket/syntax
         rebellion/base/symbol)

;@------------------------------------------------------------------------------

(define (format-one-id template id) (format-id id template id #:subs? #true))

(define (default-descriptor-identifier id) (format-one-id "descriptor:~a" id))
(define (default-predicate-name name) (format-symbol "~a?" name))
(define (default-predicate-identifier id) (format-one-id "~a?" id))
(define (default-constructor-name name) (format-symbol "constructor:~a" name))
(define (default-constructor-identifier id) (format-one-id "constructor:~a" id))
(define (default-accessor-name name) (format-symbol "accessor:~a" name))
(define (default-accessor-identifier id) (format-one-id "accessor:~a" id))

(define (default-discriminator-name name)
  (format-symbol "discriminator:~a" name))

(define (default-discriminator-identifier id)
  (format-one-id "discriminator:~a" id))

(define (default-selector-name name) (format-symbol "selector:~a" name))
(define (default-selector-identifier id) (format-one-id "selector:~a" id))
(define (default-opaque-constructor-name name) (format-symbol "make-~a" name))
(define (default-opaque-constructor-identifier id) (format-one-id "make-~a" id))
(define (default-pattern-identifier id) (format-one-id "pattern:~a" id))
(define (default-setter-identifier id) (format-one-id "~a-set" id))

(define (default-field-accessor-identifier type-id field-id)
  (format-id field-id "~a-~a" type-id field-id #:subs? #true))

(define (default-unwrapping-accessor-name name) (format-symbol "~a-value" name))

(define (default-unwrapping-accessor-identifier id)
  (format-one-id "~a-value" id))

(define (default-instance-identifier id) (format-one-id "instance:~a" id))
(define (default-renamer-identifier id) (format-one-id "~a-rename" id))
(define (default-renamer-name name) (format-symbol "~a-rename" name))
