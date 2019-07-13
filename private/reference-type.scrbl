#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     rebellion/base/symbol
                     rebellion/collection/keyset
                     rebellion/custom-write
                     rebellion/equal+hash
                     rebellion/type/reference))

@title{Reference Types}
@defmodule[rebellion/type/reference]

@defproc[(reference-type? [v any/c]) boolean?]

@defproc[(reference-type
          [name interned-symbol?]
          [fields keyset?]
          [#:object-name-field name-field natural?
           (keyset-index-of fields '#:name)]
          [#:constructor-name constructor-name (or/c interned-symbol? #f) #f]
          [#:predicate-name predicate-name (or/c interned-symbol? #f) #f]
          [#:accessor-name accessor-name (or/c interned-symbol? #f) #f])
         reference-type?]

@defform[(define-reference-type id (field-id ...) option ...)
         #:grammar
         ([option field-option naming-option property-option]
          [field-option
           (code:line)
           (code:line #:object-name-field name-field-id)]
          [naming-option
           (code:line #:predicate-name predicate-id)
           (code:line #:constructor-name constructor-id)
           (code:line #:accessor-name accessor-id)]
          [property-option
           (code:line)
           (code:line #:property-maker prop-maker-expr)])
         #:contracts
         ([prop-maker-expr (-> uninitialized-reference-descriptor?
                               (listof (cons/c struct-type-property? any/c)))])]

@defproc[(make-reference-implementation
          [type reference-type?]
          [#:property-maker prop-maker
           (-> uninitialized-reference-descriptor?
               (listof (cons/c struct-type-property? any/c)))
           make-default-reference-properties]
          [#:inspector inspector inspector? (current-inspector)])
         initialized-reference-descriptor?]

@defproc[(reference-type-name [type reference-type?]) interned-symbol?]
@defproc[(reference-type-fields [type reference-type?]) keyset?]
@defproc[(reference-type-size [type reference-type?]) natural?]
@defproc[(reference-type-object-name-field [type reference-type?]) natural?]

@defproc[(reference-type-constructor-name [type reference-type?])
         interned-symbol?]

@defproc[(reference-type-predicate-name [type reference-type?])
         interned-symbol?]

@defproc[(reference-type-accessor-name [type reference-type?])
         interned-symbol?]

@section{Reference Type Descriptors}

@defproc[(reference-descriptor? [v any/c]) boolean?]
@defproc[(initialized-reference-descriptor? [v any/c]) boolean?]
@defproc[(uninitialized-reference-descriptor? [v any/c]) boolean?]

@defproc[(reference-descriptor-type [descriptor reference-descriptor?])
         reference-type?]

@defproc[(reference-descriptor-constructor [descriptor reference-descriptor?])
         procedure?]

@defproc[(reference-descriptor-predicate [descriptor reference-descriptor?])
         predicate/c]

@defproc[(reference-descriptor-accessor [descriptor reference-descriptor?])
         (-> (reference-descriptor-predicate descriptor) natural? any/c)]

@section{Reference Type Properties}

@defproc[(make-default-reference-properties [descriptor reference-descriptor?])
         (listof (cons/c struct-type-property? any/c))]

@defproc[(make-default-reference-equal+hash [descriptor reference-descriptor?])
         equal+hash/c]

@defproc[(make-default-reference-custom-write
          [descriptor reference-descriptor?])
         custom-write-function/c]

@defproc[(make-default-reference-object-name [descriptor reference-descriptor?])
         natural?]
