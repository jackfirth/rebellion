#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     rebellion/collection/keyset
                     rebellion/type/record
                     rebellion/struct-descriptor)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/examples)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/type/record)
    #:private (list 'racket/base)))

@title{Record Types}
@defmodule[rebellion/type/record]

A @deftech{record type} is a data type representing immutable fixed-size sets of
fields, called a @deftech{record instance} of that record type. All instances of
a record type share the same set of field names, but may map those names to
different values. A record type's set of names is accessible from the type using
@racket[record-type-fields].

@defproc[(record-type? [v any/c]) boolean?]{
 A predicate for @tech{record types}.}

@defproc[(record-type [name symbol?]
                      [fields keyset?]
                      [#:predicate-name predicate-name (or/c symbol? #f) #f]
                      [#:constructor-name constructor-name (or/c symbol? #f) #f]
                      [#:accessor-name accessor-name (or/c symbol? #f) #f])
         record-type?]

@defproc[(record-type-name [type record-type?]) symbol?]
@defproc[(record-type-fields [type record-type?]) keyset?]
@defproc[(record-type-predicate-name [type record-type?]) (or/c symbol? #f)]
@defproc[(record-type-constructor-name [type record-type?]) (or/c symbol? #f)]
@defproc[(record-type-accessor-name [type record-type?]) (or/c symbol? #f)]

@defproc[(make-record-implementation
          [type record-type?]
          [#:inspector inspector inspector? (current-inspector)]
          [#:property-maker prop-maker
           (-> uninitialized-record-descriptor?
               (listof (cons/c struct-type-property? any/c)))
           make-default-record-properties])
         initialized-record-descriptor?]

@defproc[(record-descriptor? [v any/c]) boolean?]
@defproc[(initialized-record-descriptor? [v any/c]) boolean?]
@defproc[(uninitialized-record-descriptor? [v any/c]) boolean?]

@defproc[(record-descriptor-type [descriptor record-descriptor?]) record-type?]

@defproc[(record-descriptor-predicate [descriptor record-descriptor?])
         (-> any/c boolean?)]

@defproc[(record-descriptor-constructor [descriptor record-descriptor?])
         procedure?]

@defproc[(record-descriptor-accessor [descriptor record-descriptor?])
         (-> (record-descriptor-predicate descriptor) natural? any/c)]

@defproc[(make-record-field-accessor [descriptor record-descriptor?]
                                     [field natural?])
         (-> (record-descriptor-predicate descriptor) any/c)]

@defproc[(make-default-record-properties [descriptor record-descriptor?])
         (listof (cons/c struct-type-property? any/c))]

@defproc[(make-record-equal+hash [descriptor record-descriptor?]) equal+hash/c]

@defproc[(make-record-custom-write [descriptor record-descriptor?])
         custom-write-function/c]

@defform[(define-record-type id (field-id ...) option ...)
         #:grammar ([option
                     (code:line #:constructor-name constructor-id)
                     (code:line #:predicate-name predicate-id)
                     (code:line #:property-maker prop-maker-expr)])
         #:contracts
         ([prop-maker-expr
           (-> uninitialized-record-descriptor?
               (listof (cons/c struct-type-property? any/c)))])]
