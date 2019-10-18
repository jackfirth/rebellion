#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [constructor-call-markup
   (-> #:type-name interned-symbol?
       #:subexpressions list?
       constructor-call-markup?)]
  [constructor-call-markup? predicate/c]
  [keyword-markup (-> keyword? keyword-markup?)]
  [keyword-markup? predicate/c]))

(require racket/struct
         rebellion/base/symbol
         rebellion/collection/keyset
         rebellion/type/record
         rebellion/type/wrapper)

(module+ test
  (require (submod "..")
           racket/format
           rackunit))

;@------------------------------------------------------------------------------

(define ((make-markup-based-printer markup-getter) this out mode)
  (define markup (markup-getter this))
  (case mode
    [(#t) (write markup out)]
    [(#f) (display markup out)]
    [(0 1) (print markup out mode)]
    [else
     (raise-argument-error 'make-markup-based-printer
                           "(or/c #t #f 0 1)"
                           mode)]))

(define (make-constructor-call-markup-properties descriptor)
  (define accessor (record-descriptor-accessor descriptor))
  (define fields (record-type-fields (record-descriptor-type descriptor)))
  (define type-name-field (keyset-index-of fields '#:type-name))
  (define subexpressions-field (keyset-index-of fields '#:subexpressions))
  (define custom-write
    (make-constructor-style-printer
     (λ (this) (accessor this type-name-field))
     (λ (this) (accessor this subexpressions-field))))
  (list (cons prop:custom-write custom-write)
        (cons prop:equal+hash (default-record-equal+hash descriptor))))

(define (make-keyword-markup-properties descriptor)
  (define accessor (wrapper-descriptor-accessor descriptor))
  (define (markup this)
    (unquoted-printing-string
     (string-append "#:" (keyword->string (accessor this)))))
  (list (cons prop:custom-write (make-markup-based-printer markup))
        (cons prop:equal+hash (default-wrapper-equal+hash descriptor))))

(define-record-type constructor-call-markup (type-name subexpressions)
  #:property-maker make-constructor-call-markup-properties)

(define-wrapper-type keyword-markup
  #:property-maker make-keyword-markup-properties)

(module+ test
  (test-case "constructor-call-markup"
    (check-equal? (~v (constructor-call-markup #:type-name 'foo
                                               #:subexpressions (list 1 2 3)))
                  "(foo 1 2 3)"))
  (test-case "keyword-markup"
    (define kw (keyword-markup '#:foo))
    (check-equal? (~a kw) "#:foo")
    (check-equal? (~s kw) "#:foo")
    (check-equal? (~v kw) "#:foo")))
