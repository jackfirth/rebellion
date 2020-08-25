#lang racket/base

(provide define-enum-type
         enum-out)

(require (for-syntax racket/base
                     racket/provide-transform
                     racket/sequence
                     racket/set
                     racket/syntax
                     rebellion/collection/keyset/low-dependency
                     rebellion/type/enum/base
                     rebellion/private/enum-type-binding
                     (submod rebellion/private/enum-type-binding
                             private-constructor))
         rebellion/collection/keyset/low-dependency
         rebellion/type/enum/base
         rebellion/type/enum/descriptor
         syntax/parse/define)

(module+ test
  (require (for-syntax rebellion/type/enum/binding)
           (submod "..")
           racket/format
           racket/set
           rackunit
           rebellion/private/static-name))

;@------------------------------------------------------------------------------

(begin-for-syntax
  (define-syntax-class enum-constants
    #:description "enum constants"
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
      "duplicate enum constants are not allowed")))

(define-simple-macro
  (define-enum-type id:id constants:enum-constants
    (~alt
     (~optional
      (~seq #:predicate-name predicate:id)
      #:defaults
      ([predicate (format-id #'id "~a?" #'id #:subs? #t)])
      #:name "#:predicate-name option")
     
     (~optional
      (~seq #:descriptor-name descriptor:id)
      #:defaults
      ([descriptor (format-id #'id "descriptor:~a" #'id #:subs? #t)])
      #:name "#:descriptor-name option")
     
     (~optional
      (~seq #:discriminator-name discriminator:id)
      #:defaults
      ([discriminator (format-id #'id "discriminator:~a" #'id #:subs? #t)])
      #:name "#:discriminator-name option")
     
     (~optional
      (~seq #:selector-name selector:id)
      #:defaults
      ([selector (format-id #'id "selector:~a" #'id #:subs? #t)])
      #:name "#:selector-name option")
     
     (~optional
      (~seq #:property-maker prop-maker:expr)
      #:defaults ([prop-maker #'default-enum-properties])
      #:name "#:property-maker option"))
    ...)
  (begin
    (define type
      (enum-type 'id constants.names #:predicate-name 'predicate))
    (define descriptor
      (make-enum-implementation type #:property-maker prop-maker))
    (define predicate (enum-descriptor-predicate descriptor))
    (define discriminator (enum-descriptor-discriminator descriptor))
    (define selector (enum-descriptor-selector descriptor))
    (define constants.id (selector constants.index))
    ...
    (define-syntax id
      (enum-binding
       #:type (enum-type 'id constants.names #:predicate-name 'predicate)
       #:constants (set #'constants.id ...)
       #:descriptor #'descriptor
       #:predicate #'predicate
       #:discriminator #'discriminator
       #:selector #'selector))))

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
    (check-equal? (~s west) "#<compass-direction:west>")
    (define-simple-macro (enum-constants enum:enum-id)
      (set enum.constant ...))
    (check-equal? (enum-constants compass-direction)
                  (set north south east west))))

(define-syntax enum-out
  (make-provide-transformer
   (λ (provide-spec modes)
     (syntax-parse provide-spec
       [(_ enum:enum-id)
        (expand-export
         #'(combine-out enum enum.predicate enum.constant ...) modes)]))))

(module+ test
  (provide (enum-out direction))
  (define-enum-type direction (up down left right)))
