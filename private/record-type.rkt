#lang racket/base

(provide define-record-setter
         define-record-type)

(require (for-syntax racket/base
                     racket/sequence
                     racket/syntax
                     rebellion/collection/keyset/low-dependency
                     rebellion/private/record-type-binding
                     (submod rebellion/private/record-type-binding
                             private-constructor)
                     rebellion/type/record/base
                     syntax/transformer)
         racket/match
         rebellion/collection/keyset/low-dependency
         rebellion/private/static-name
         rebellion/type/record/base
         rebellion/type/record/descriptor
         syntax/parse/define)

(module+ test
  (require (submod "..")
           racket/format
           rackunit))

;@------------------------------------------------------------------------------

(begin-for-syntax
  (define-syntax-class record-fields
    #:attributes ([id 1] [keyword 1] keys [position 1])
    (pattern (unsorted-id:id ...)
      #:with (id ...)
      (sort (syntax->list #'(unsorted-id ...)) symbol<? #:key syntax-e)
      #:with (keyword ...)
      (for/list ([id-stx (in-syntax #'(id ...))])
        (string->keyword (symbol->string (syntax-e id-stx))))
      #:with keys #'(keyset keyword ...)
      #:with (position ...)
      (for/list ([_ (in-syntax #'(id ...))] [n (in-naturals)]) #`'#,n))))

(define-simple-macro
  (define-record-type id:id fields:record-fields
    (~alt
     (~optional (~and #:omit-root-binding omit-root-binding-kw))
     (~optional
      (~seq #:predicate-name predicate:id)
      #:defaults ([predicate (format-id #'id "~a?" #'id #:subs? #t)])
      #:name "#:predicate-name option")
     (~optional
      (~seq #:descriptor-name descriptor:id)
      #:defaults ([descriptor (format-id #'id "descriptor:~a" #'id #:subs? #t)])
      #:name "#:descriptor-name option")
     (~optional
      (~seq #:constructor-name constructor:id)
      #:defaults
      ([constructor (format-id #'id "constructor:~a" #'id #:subs? #t)])
      #:name "#:constructor-name option")
     (~optional
      (~seq #:accessor-name accessor:id)
      #:defaults
      ([accessor (format-id #'id "accessor:~a" #'id #:subs? #t)])
      #:name "#:accessor-name option")
     (~optional
      (~seq #:pattern-name pattern:id)
      #:defaults ([pattern (format-id #'id "pattern:~a" #'id #:subs? #t)])
      #:name "#:pattern-name option")
     (~optional
      (~seq #:property-maker prop-maker:expr)
      #:defaults ([prop-maker #'default-record-properties])
      #:name "#:property-maker option"))
    ...)
  #:with (field-accessor ...)
  (for/list ([field-id-stx (in-syntax #'(fields.id ...))])
    (format-id #'id "~a-~a" #'id field-id-stx #:subs? #t))
  #:with root-binding
  (if (attribute omit-root-binding-kw)
      #'(begin)
      #'(define-syntax id
          (record-binding
           #:type
           (record-type
            'id fields.keys
            #:predicate-name 'predicate
            #:constructor-name 'constructor)
           #:descriptor #'descriptor
           #:predicate #'predicate
           #:constructor #'constructor
           #:accessor #'accessor
           #:fields (list #'fields.id ...)
           #:field-accessors (list #'field-accessor ...)
           #:pattern #'pattern
           #:macro (make-variable-like-transformer #'constructor))))
  (begin
    (define type
      (record-type 'id fields.keys
                   #:predicate-name 'predicate
                   #:constructor-name 'constructor))
    (define descriptor
      (make-record-implementation type #:property-maker prop-maker))
    (define predicate (record-descriptor-predicate descriptor))
    (define constructor (record-descriptor-constructor descriptor))
    (define accessor (record-descriptor-accessor descriptor))
    (define field-accessor
      (make-record-field-accessor descriptor fields.position))
    ...
    (define-match-expander pattern
      (syntax-parser
        #:track-literals
        [(_ (~alt (~optional (~seq fields.keyword fields.id)) ...) (... ...))
         #'(? predicate ((... ~?) (app field-accessor fields.id)) ...)]))
    root-binding))

(module+ test
  (test-case (name-string define-record-type)
    (define-record-type person (name age favorite-color))
    (define ted (person #:name "Ted" #:age 42 #:favorite-color 'grey))
    (check-equal? ted (person #:age 42 #:name "Ted" #:favorite-color 'grey))
    (check-equal? (person-name ted) "Ted")
    (check-equal? (person-age ted) 42)
    (check-equal? (person-favorite-color ted) 'grey)
    (check-true (person? ted))
    (check-match ted (person #:age 42))
    (check-match ted (person #:name (? string?) #:favorite-color 'grey))
    (define-record-type plant (name))
    (check-false (person? (plant #:name "Cactus")))
    (check-equal?
     (~a ted) "#<person: #:age 42 #:favorite-color grey #:name Ted>")
    (check-equal?
     (~v ted) "(person #:age 42 #:favorite-color 'grey #:name \"Ted\")")
    (check-equal?
     (~s ted) "#<person: #:age 42 #:favorite-color grey #:name \"Ted\">")))


(define-simple-macro
  (define-record-setter record:record-id
    (~optional
     setter:id
     #:defaults ([setter (format-id #'record "~a-set" #'record #:subs? #t)])))
  (define (setter instance
                  (~@ record.field-keyword
                      [record.field (record.field-accessor instance)]) ...)
    (record.constructor (~@ record.field-keyword record.field) ...)))

(module+ test
  (test-case (name-string define-record-setter)
    (define-record-type person (name age favorite-color))
    (define-record-setter person)
    (define ted (person #:name "Ted" #:age 42 #:favorite-color 'grey))
    (check-equal?
     (person-set ted #:name "Joe")
     (person #:name "Joe" #:age 42 #:favorite-color 'grey))
    (check-equal? (person-set ted) ted)
    (check-equal?
     (person-set ted #:name "Joe" #:age 0 #:favorite-color 'white)
     (person #:name "Joe" #:age 0 #:favorite-color 'white))

    (test-case "name collision"
      (define-record-type tester (instance))
      (define-record-setter tester)
      (check-equal?
       (tester-set (tester #:instance 1) #:instance 2) (tester #:instance 2))
      (check-equal? (tester-set (tester #:instance 1)) (tester #:instance 1)))))
