#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     rebellion/base/symbol
                     rebellion/collection/keyset
                     rebellion/custom-write
                     rebellion/equal+hash
                     rebellion/type/object)
          (submod rebellion/private/scribble-cross-document-tech doc))

@title{Reference Types}
@defmodule[rebellion/type/object]

@defform[(define-reference-type id (field-id ...) option ...)
         #:grammar
         ([option field-option naming-option property-option]
          [field-option
           (code:line)
           (code:line #:object-name-field name-field-id)]
          [naming-option
           (code:line #:predicate-name predicate-id)
           (code:line #:constructor-name constructor-id)
           (code:line #:accessor-name accessor-id)
           (code:line #:descriptor-name descriptor-id)]
          [property-option
           (code:line)
           (code:line #:property-maker prop-maker-expr)])
         #:contracts
         ([prop-maker-expr (-> uninitialized-reference-descriptor?
                               (listof (cons/c struct-type-property? any/c)))])]

@section{Reference Type Information}

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

@section{Dynamically Implementing Reference Types}

@defproc[(make-reference-implementation
          [type reference-type?]
          [#:property-maker prop-maker
           (-> uninitialized-reference-descriptor?
               (listof (cons/c struct-type-property? any/c)))
           default-reference-properties]
          [#:inspector inspector inspector? (current-inspector)])
         initialized-reference-descriptor?]

@defproc[(default-reference-properties [descriptor reference-descriptor?])
         (listof (cons/c struct-type-property? any/c))]

@defproc[(default-reference-equal+hash [descriptor reference-descriptor?])
         equal+hash/c]

@defproc[(default-reference-custom-write
          [descriptor reference-descriptor?])
         custom-write-function/c]

@defproc[(default-reference-object-name [descriptor reference-descriptor?])
         natural?]

@section{Reference Type Chaperones and Impersonators}

@defproc[(reference-impersonate
          [instance (reference-descriptor-predicate descriptor)]
          [descriptor initialized-reference-descriptor?]
          [#:properties properties
           (hash/c impersonator-property? any/c #:immutable #t)
           empty-hash]
          [#:chaperone? chaperone? boolean? #t])
         (reference-descriptor-predicate descriptor)]{
 Returns an @tech/reference{impersonator} of @racket[instance] with each
 @tech/reference{impersonator property} in @racket[properties] attached to it.
 If @racket[chaperone?] is true (the default), the returned impersonator is a
 @tech/reference{chaperone}.}
