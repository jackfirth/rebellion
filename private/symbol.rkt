#lang rebellion/private/dependencies/layer1

(provide
 symbol
 (contract-out
  [symbol? predicate?]
  [interned-symbol? predicate?]
  [uninterned-symbol? predicate?]
  [unreadable-symbol? predicate?]))

(require rebellion/private/boolean
         rebellion/name
         rebellion/predicate
         syntax/parse/define
         (only-in racket/base
                  [quote racket:quote]
                  [symbol? racket:symbol?]
                  [symbol-interned? racket:symbol-interned?]
                  [symbol-unreadable? racket:symbol-unreadable?]))

;@------------------------------------------------------------------------------

(define-simple-macro (symbol id:id) (racket:quote id))

(define (plain-interned-symbol? v)
  (and (racket:symbol? v) (racket:symbol-interned? v)))

(define (plain-uninterned-symbol? v)
  (and (racket:symbol? v)
       (not (racket:symbol-interned? v))
       (not (racket:symbol-unreadable? v))))

(define (plain-unreadable-symbol? v)
  (and (racket:symbol? v) (racket:symbol-unreadable? v)))

(define-simple-macro (define-predicates [pred-id:id proc-expr:expr] ...)
  (define-values (pred-id ...)
    (values (make-predicate proc-expr #:name (symbolic-name (symbol pred-id)))
            ...)))

(define-predicates
  [symbol? racket:symbol?]
  [interned-symbol? plain-interned-symbol?]
  [uninterned-symbol? plain-uninterned-symbol?]
  [unreadable-symbol? plain-unreadable-symbol?])
