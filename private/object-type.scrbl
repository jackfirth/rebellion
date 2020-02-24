#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     rebellion/base/symbol
                     rebellion/collection/hash
                     rebellion/collection/keyset
                     rebellion/custom-write
                     rebellion/equal+hash
                     rebellion/type/object)
          (submod rebellion/private/scribble-cross-document-tech doc))

@title{Object Types}
@defmodule[rebellion/type/object]

@defform[(define-object-type id (field-id ...) option ...)
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
         ([prop-maker-expr (-> uninitialized-object-descriptor?
                               (listof (cons/c struct-type-property? any/c)))])]

@section{Object Type Information}

@defproc[(object-type? [v any/c]) boolean?]

@defproc[(object-type
          [name interned-symbol?]
          [fields keyset?]
          [#:object-name-field name-field natural?
           (keyset-index-of fields '#:name)]
          [#:constructor-name constructor-name (or/c interned-symbol? #f) #f]
          [#:predicate-name predicate-name (or/c interned-symbol? #f) #f]
          [#:accessor-name accessor-name (or/c interned-symbol? #f) #f])
         object-type?]

@defproc[(object-type-name [type object-type?]) interned-symbol?]
@defproc[(object-type-fields [type object-type?]) keyset?]
@defproc[(object-type-size [type object-type?]) natural?]
@defproc[(object-type-object-name-field [type object-type?]) natural?]
@defproc[(object-type-constructor-name [type object-type?]) interned-symbol?]
@defproc[(object-type-predicate-name [type object-type?]) interned-symbol?]
@defproc[(object-type-accessor-name [type object-type?]) interned-symbol?]

@section{Object Type Descriptors}

@defproc[(object-descriptor? [v any/c]) boolean?]
@defproc[(initialized-object-descriptor? [v any/c]) boolean?]
@defproc[(uninitialized-object-descriptor? [v any/c]) boolean?]

@defproc[(object-descriptor-type [descriptor object-descriptor?]) object-type?]

@defproc[(object-descriptor-constructor [descriptor object-descriptor?])
         procedure?]

@defproc[(object-descriptor-predicate [descriptor object-descriptor?])
         predicate/c]

@defproc[(object-descriptor-accessor [descriptor object-descriptor?])
         (-> (object-descriptor-predicate descriptor) natural? any/c)]

@section{Dynamically Implementing Object Types}

@defproc[(make-object-implementation
          [type object-type?]
          [#:property-maker prop-maker
           (-> uninitialized-object-descriptor?
               (listof (cons/c struct-type-property? any/c)))
           default-object-properties]
          [#:inspector inspector inspector? (current-inspector)])
         initialized-object-descriptor?]

@defproc[(default-object-properties [descriptor object-descriptor?])
         (listof (cons/c struct-type-property? any/c))]

@defproc[(default-object-equal+hash [descriptor object-descriptor?])
         equal+hash/c]

@defproc[(default-object-custom-write [descriptor object-descriptor?])
         custom-write-function/c]

@defproc[(default-object-name-property [descriptor object-descriptor?])
         natural?]

@section{Object Type Chaperones and Impersonators}

@defproc[(object-impersonate
          [instance (object-descriptor-predicate descriptor)]
          [descriptor initialized-object-descriptor?]
          [#:properties properties
           (hash/c impersonator-property? any/c #:immutable #t)
           empty-hash]
          [#:chaperone? chaperone? boolean? #t])
         (object-descriptor-predicate descriptor)]{
 Returns an @tech/reference{impersonator} of @racket[instance] with each
 @tech/reference{impersonator property} in @racket[properties] attached to it.
 If @racket[chaperone?] is true (the default), the returned impersonator is a
 @tech/reference{chaperone}.}
