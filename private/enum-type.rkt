#lang racket/base

(provide define-enum-type)

(require (for-syntax racket/base
                     racket/sequence
                     racket/syntax)
         racket/match
         rebellion/collection/keyset/low-dependency
         rebellion/type/enum/base
         rebellion/type/enum/descriptor
         syntax/parse/define)

(module+ test
  (require (submod "..")
           racket/format
           rackunit
           rebellion/private/static-name))

;@------------------------------------------------------------------------------

(begin-for-syntax
  (define-syntax-class enum-cases
    #:description "enum cases"
    #:attributes (names [id 1] [index 1])
    (pattern (unsorted-id:id ...)
      
      #:with (id ...)
      (sort (syntax->list #'(unsorted-id ...)) symbol<? #:key syntax-e)

      #:with (kw ...)
      (for/list ([case (in-syntax #'(id ...))])
        (string->keyword (symbol->string (syntax-e case))))

      #:with names #'(keyset kw ...)
      #:with (index ...)
      (for/list ([n (in-range 0 (sequence-length (in-syntax #'(id ...))))])
        #`'#,n)

      #:fail-when
      (check-duplicate-identifier (syntax->list #'(unsorted-id ...)))
      "duplicate enum cases are not allowed")))

(define-simple-macro (define-enum-case case-id selector case-index)
  #:with case-value (format-id #'case-id "~a-val" #'case-id)
  (begin
    (define case-value (selector case-index))
    (define-match-expander case-id
      (lambda (stx) #'(== case-value))
      (make-rename-transformer #'case-value))))

(define-simple-macro
  (define-enum-type id:id cases:enum-cases
    (~alt
     (~optional (~seq #:predicate-name predicate-name:id)
                #:defaults
                ([predicate-name (format-id #'id "~a?" #'id #:subs? #t)])
                #:name "#:predicate-name option")
     (~optional (~seq #:property-maker prop-maker:expr)
                #:defaults ([prop-maker #'default-enum-properties])
                #:name "#:property-maker option"))
    ...)
  (begin
    (define type (enum-type 'id cases.names #:predicate-name 'predicate-name))
    (define descriptor
      (make-enum-implementation type #:property-maker prop-maker))
    (define predicate-name (enum-descriptor-predicate descriptor))
    (define selector (enum-descriptor-selector descriptor))
    (define-enum-case cases.id selector cases.index)
    ...))

(module+ test
  (test-case (name-string define-enum-type)
    (define-enum-type compass-direction (north south east west))
    (check-pred compass-direction? north)
    (check-pred compass-direction? south)
    (check-pred compass-direction? east)
    (check-pred compass-direction? west)
    (check-not-equal? north south)
    (check-false (compass-direction? 42))
    (check-equal? (object-name compass-direction?) (name compass-direction?))
    (check-equal? (object-name north) (name north))
    (check-equal? (~a south) "#<compass-direction:south>")
    (check-equal? (~v east) "#<compass-direction:east>")
    (check-equal? (~s west) "#<compass-direction:west>")))
