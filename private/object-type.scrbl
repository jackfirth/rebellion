#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     rebellion/base/symbol
                     rebellion/collection/hash
                     rebellion/collection/keyset
                     rebellion/custom-write
                     rebellion/equal+hash
                     rebellion/type/object
                     rebellion/type/object/binding
                     syntax/parse/define)
          (submod rebellion/private/scribble-cross-document-tech doc)
          (submod rebellion/private/scribble-evaluator-factory doc)
          (submod rebellion/private/scribble-index-attribute doc)
          scribble/examples)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/type/object
                   'syntax/parse/define)
    #:private (list 'racket/base)))

@title{Object Types}
@defmodule[rebellion/type/object]

An @deftech{object type} is a kind of @tech{data type} for opaque named values.
Object types are similar to @tech{record types} in that they both have an
unordered set of fields and a keyword-based constructor. However, object types
are intended for encapsulating behavior, not data. In support of this, object
types have an automatic @racket[#:name] field used to implement
@racket[prop:object-name]. Instead of printing their fields, instances of object
types print like other named opaque values such as functions and ports and print
only their names.

As a rule of thumb, if you want to simply bundle together data then use record
types. But if you want to group together functions or other complex objects, and
you don't want to give users access to them, use object types. Examples of
object types in Rebellion include @tech{converters}, @tech{comparators},
@tech{reducers}, and @tech{transducers}.

@defform[
 (define-object-type id (field-id ...) option ...)
 #:grammar
 ([option
   (code:line #:descriptor-name descriptor-id)
   (code:line #:predicate-name predicate-id)
   (code:line #:constructor-name constructor-id)
   (code:line #:accessor-name accessor-id)
   (code:line #:property-maker prop-maker-expr)])
 #:contracts
 ([prop-maker-expr
   (-> uninitialized-object-descriptor?
       (listof (cons/c struct-type-property? any/c)))])]{
 Creates a new @tech{object type} named @racket[id] and binds the following
 identifiers:

 @itemlist[
 @item{@racket[constructor-id], which defaults to
   @racketidfont{make-}@racket[id] --- a constructor function that accepts one
   mandatory keyword argument for each @racket[field-id] and one optional
   @racket[#:name] argument, then constructs an instance with the given name.
   The name argument must be either an interned symbol or false (the default).
   If the name argument is false, then the constructed instance is anonymous and
   its printed representation will not include a name.}

 @item{@racket[predicate-id], which defaults to @racket[id]@racketidfont{?} ---
   a predicate function that returns @racket[#t] when given instances of the
   created type and returns @racket[#f] otherwise.}

 @item{@racket[id]@racketidfont{-}@racket[field-id] for each @racket[field-id]
   --- an accessor function that returns the value for @racket[field-id] when
   given an instance of the created type.}]

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define-object-type folder (accumulator initial-state))

    (define (fold sequence folder)
      (define initial-state (folder-initial-state folder))
      (define accumulator (folder-accumulator folder))
      (for/fold ([state initial-state]) ([v sequence])
        (accumulator state v)))
   
    (define sum
      (make-folder #:accumulator +
                   #:initial-state 0
                   #:name 'sum)))

   sum
   (fold (list 1 2 3 4) sum))}

@section{Object Type Information}

@defproc[(object-type? [v any/c]) boolean?]{
 A predicate for @tech{object types}.}

@defproc[(object-type
          [name interned-symbol?]
          [fields keyset?]
          [#:name-field name-field keyword? '#:name]
          [#:constructor-name constructor-name (or/c interned-symbol? #f) #f]
          [#:predicate-name predicate-name (or/c interned-symbol? #f) #f]
          [#:accessor-name accessor-name (or/c interned-symbol? #f) #f])
         object-type?]{
 Constructs an @tech{object type} named @racket[name] and with fields named
 @racket[fields], with @racket[name-field] used to store the names of instances
 of the type.

 The optional @racket[predicate-name], @racket[constructor-name], and
 @racket[accessor-name] arguments control the result of @racket[object-name] on
 the functions implementing the type. If not provided, @racket[predicate-name]
 defaults to @racket[name]@racketidfont{?}, @racket[constructor-name] defaults
 to @racketidfont{make-}@racket[name], and @racket[accessor-name] defaults to
 @racket[name]@racketidfont{-ref}.

 This function only constructs the information representing an object type; to
 implement the type, use @racket[make-object-implementation].}

@defproc[(object-type-name [type object-type?]) interned-symbol?]{
 Returns the name of @racket[type].}

@defproc[(object-type-fields [type object-type?]) keyset?]{
 Returns the set of fields in @racket[type], including the name field.}

@defproc[(object-type-private-fields [type object-type?]) keyset?]{
 Returns the set of fields in @racket[type] that are hidden from users --- that
 is, every field except for the name field.}

@defproc[(object-type-name-field [type object-type?]) keyword?]{
 Returns the name field of @racket[type].}

@defproc[(object-type-name-field-position [type object-type?]) natural?]{
 Returns the position of the name field of @racket[type].}

@defproc[(object-type-constructor-name [type object-type?]) interned-symbol?]{
 Returns the name of the constructor for @racket[type].}

@defproc[(object-type-predicate-name [type object-type?]) interned-symbol?]{
 Returns the name of the predicate for @racket[type].}

@defproc[(object-type-accessor-name [type object-type?]) interned-symbol?]{
 Returns the name of the accessor for @racket[type].}

@defproc[(object-type-size [type object-type?]) natural?]{
 Returns the number of fields (include the name field) in @racket[type].}

@section{Object Type Descriptors}

The @tech{type descriptor} for an @tech{object type} contains two functions that
implement the type:

@itemlist[
 @item{An @deftech{object constructor} that accepts one argument for each field
  of the object type and constructs an object instance.}

 @item{An @deftech{object accessor} that accepts an instance of the object type
  and an integer field index, then returns the value of the corresponding
  field.}]

These functions can be used to dynamically construct and inspect instances of
arbitrary object types at runtime, assuming the type's descriptor is
initialized. Note that the descriptor contains a single accessor function that
can access any field in the object: the per-field accessors created by
@racket[define-object-type] are merely convenient wrappers around this accessor.

@defproc[(object-descriptor? [v any/c]) boolean?]{
 A predicate for object @tech{type descriptors}.}

@defproc[(initialized-object-descriptor? [v any/c]) boolean?]{
 A predicate for initialized object @tech{type descriptors}.}

@defproc[(uninitialized-object-descriptor? [v any/c]) boolean?]{
 A predicate for uninitialized object @tech{type descriptors}.}

@defproc[(make-object-implementation
          [type object-type?]
          [#:inspector inspector inspector? (current-inspector)]
          [#:property-maker prop-maker
           (-> uninitialized-object-descriptor?
               (listof (cons/c struct-type-property? any/c)))
           default-object-properties])
         initialized-object-descriptor?]{
 Implements @racket[type] and returns a @tech{type descriptor} for the new
 implementation. The @racket[inspector] argument behaves the same as in
 @racket[make-struct-type], although there are no transparent or prefab object
 types. The @racket[prop-maker] argument is similar to the corresponding
 argument of @racket[make-struct-implementation]. By default, object types are
 created with properties that make them print like opaque named objects.}

@defproc[(object-descriptor-type [descriptor object-descriptor?]) object-type?]{
 Returns the @tech{object type} that @racket[descriptor] implements.}

@defproc[(object-descriptor-predicate [descriptor object-descriptor?])
         predicate/c]{
 Returns a predicate that returns true when given any object instance created by
 @racket[descriptor]. The predicate is specific to @racket[descriptor] --- it
 will not return true for object instances created by any other object
 descriptors, even if they're different implementations of the same object type
 as @racket[descriptor]. This is because object types are @tech{nominal types}.}

@defproc[(object-descriptor-constructor [descriptor object-descriptor?])
         procedure?]{
 Returns the @tech{object constructor} of the object type implementation
 represented by @racket[descriptor]. The constructor accepts one keyword
 argument for each field in the object type and returns an instance of the
 object type. All of the constructor's keyword arguments are mandatory, except
 for the argument that supplies the instance's name --- that argument defaults
 to @racket[#f] if not given.}

@defproc[(object-descriptor-accessor [descriptor object-descriptor?])
         (-> (object-descriptor-predicate descriptor) natural? any/c)]{
 Returns the @tech{object accessor} of the object type implementation
 represented by @racket[descriptor]. The accessor accepts two arguments: an
 object instance created by @racket[descriptor], and a nonnegative integer less
 than the number of fields in the object type. The accessor returns the value of
 the corresponding field in the instance. To access fields by name, first
 determine the integer index of the field name using the @tech{keyset} returned
 by @racket[object-type-fields]. See also @racket[make-object-field-accessor] to
 construct a field-specific accessor function.}

@defproc[(make-object-field-accessor [descriptor object-descriptor?]
                                     [field natural?])
         (-> (object-descriptor-predicate descriptor) any/c)]{
 Constructs a function that accepts an instance of the object type implemented
 by @racket[descriptor] and returns the value of the field at position
 @racket[field] in the object.}

@defproc[(default-object-properties [descriptor object-descriptor?])
         (listof (cons/c struct-type-property? any/c))]{
 Returns implementations of @racket[prop:equal+hash], @racket[prop:object-name],
 and @racket[prop:custom-write] suitable for most @tech{object types}. This
 function is called by @racket[make-object-implementation] when no
 @racket[_prop-maker] argument is supplied.}

@defproc[(default-object-equal+hash [descriptor object-descriptor?])
         equal+hash/c]{
 Builds an equality-checking function, a hashing function, and a secondary
 hashing function suitable for use with @racket[prop:equal+hash], each of which
 operate on object instances created by @racket[descriptor]. Two instances are
 equal if and only if the fields of one instance are equal to the fields of the
 other. This function is used by @racket[default-object-properties] to implement
 @racket[prop:equal+hash].}

@defproc[(default-object-custom-write [descriptor object-descriptor?])
         custom-write-function/c]{
 Builds a @tech{custom write implementation} that prints object instances
 created by @racket[descriptor] in a manner similar to other opaque named Racket
 values, such as functions and ports. Instances are printed as unreadable values
 and include the name of the instance prefixed by the name of the instance's
 type. Anonymous instances only print their type. This function is used by
 @racket[default-object-properties] to implement @racket[prop:custom-write].}

@defproc[(default-object-name-property [descriptor object-descriptor?])
         natural?]{
 Builds a value suitable for use with @racket[prop:object-name] that causes
 @racket[object-name] to return the name of the instance when used on object
 instances created by @racket[descriptor]. This function is used by
 @racket[default-object-properties] to implement @racket[prop:object-name].}

@section{Object Type Bindings}
@defmodule[rebellion/type/object/binding]

An @deftech{object type binding} is a @tech{type binding} for an
@tech{object type}. Object type bindings contain compile-time information about
the object type's name and fields, as well as runtime bindings for its
predicate, @tech{type descriptor}, and other runtime components. To extract an
object type binding bound by @racket[define-object-type], use the
@racket[object-id] @syntax-tech{syntax class}.

@defproc[(object-binding? [v any/c]) boolean?]{
 A predicate for @tech{object type bindings}.}

@defidform[#:kind "syntax class" object-id]{
 A @syntax-tech{syntax class} for @tech{object type bindings} bound by
 @racket[define-object-type]. This class matches any @tech/reference{identifier}
 bound with @racket[define-syntax] to a value satisfying the
 @racket[object-binding?] predicate, similar to the @racket[static] syntax
 class. Upon a successful match, the @racket[object-id] class defines the
 following attributes:

 @itemlist[

 @item{@index-attribute[object-id type] --- an attribute bound to a compile-time
   @racket[object-type?] value describing the type.}

 @item{@index-attribute[object-id binding] --- an attribute bound to the
   compile-time @racket[object-binding?] value extracted from the matched
   identifier.}

 @item{@index-attribute[object-id name] --- a pattern variable bound to the
   object type's name, as a quoted symbol.}

 @item{@index-attribute[object-id descriptor] --- a pattern variable bound to
   the object type's runtime @tech{type descriptor}.}

 @item{@index-attribute[object-id predicate] --- a pattern variable bound to the
   object type's runtime type predicate.}

 @item{@index-attribute[object-id constructor] --- a pattern variable bound to
   the object type's runtime @tech{object constructor}.}

 @item{@index-attribute[object-id accessor] --- a pattern variable bound to the
   object type's runtime @tech{object accessor}.}

 @item{@index-attribute[object-id field ...] --- a pattern variable bound to the
   object's field identifiers. This includes both the object type's private
   fields and its name field.}

 @item{@index-attribute[object-id field-name ...] --- a pattern variable bound
   to the object's field names, as quoted symbols. This includes both the object
   type's private fields and its name field.}

 @item{@index-attribute[object-id field-keywords ...] --- a pattern variable
   bound to the object's field names, as keywords. This includes both the object
   type's private fields and its name field.}

 @item{@index-attribute[object-id field-accessor ...] --- a pattern variable
   bound to the object type's per-field runtime field accessors. This includes
   accessors for both the private fields and the name field.}

 @item{@index-attribute[object-id private-field ...] --- a pattern variable
   bound to the object's private field identifiers. The name field is not a
   private field.}

 @item{@index-attribute[object-id private-field-name ...] --- a pattern variable
   bound to the object's private field names, as quoted symbols. The name field
   is not a private field.}

 @item{@index-attribute[object-id private-field-keyword ...] --- a pattern
   variable bound to the object's private field names, as keywords. The name
   field is not a private field.}

 @item{@index-attribute[object-id private-accessor ...] --- a pattern variable
   bound to the object type's per-field runtime private field accessors. This
   does not include an accessor for the name field.}

 @item{@index-attribute[object-id name-field] --- a pattern variable bound to
   the object's name field identifier.}

 @item{@index-attribute[object-id name-field-name] --- a pattern variable bound
   to the object's name field, as a quoted symbol.}

 @item{@index-attribute[object-id name-field-keyword] --- a pattern variable
   bound to the object's name field, as a keyword.}

 @item{@index-attribute[object-id name-accessor] --- a pattern variable
   bound to the object type's name field accessor.}]

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (require (for-syntax rebellion/type/object/binding))
    
    (define-simple-macro (object-accessors object:object-id)
      (list object.field-accessor ...))

    (define-object-type folder (accumulator initial-state)))

   (object-accessors folder))}

@defproc[(object-binding-type [binding object-binding?]) object-type?]{
 Returns the @tech{object type} that @racket[binding] is for. When an object
 type binding is bound with @racket[define-syntax], this can be used at
 compile-time to obtain information about the name and fields of the object
 type.}

@defproc[(object-binding-descriptor [binding object-binding?]) identifier?]{
 Returns an identifier that is bound at runtime to the @tech{type descriptor}
 for the object type bound by @racket[binding]. When an object type binding is
 bound with @racket[define-syntax], this can be used in macro-generated code to
 work with object types dynamically.}

@defproc[(object-binding-predicate [binding object-binding?]) identifier?]{
 Returns an identifier that is bound at runtime to the predicate for the object
 type bound by @racket[binding].}

@defproc[(object-binding-constructor [binding object-binding?]) identifier?]{
 Returns an identifier that is bound at runtime to the @tech{object constructor}
 for the object type bound by @racket[binding].}

@defproc[(object-binding-accessor [binding object-binding?]) identifier?]{
 Returns an identifier that is bound at runtime to the @tech{object accessor}
 for the object type bound by @racket[binding].}

@defproc[(object-binding-fields [binding object-binding?])
         (vectorof identifier? #:immutable #t)]{
 Returns a vector of the identifiers used for the fields of the object type
 bound by @racket[binding]. This includes both the private fields and the name
 field.}

@defproc[(object-binding-field-accessors [binding object-binding?])
         (vectorof identifier? #:immutable #t)]{
 Returns a vector of identifiers that are bound at runtime to the per-field
 accessors of the object type bound by @racket[binding]. This includes accessors
 for both the private fields and the name field.}

@defproc[(object-binding-private-fields [binding object-binding?])
         (vectorof identifier? #:immutable #t)]{
 Returns a vector of the identifiers used for the private fields of the object
 type bound by @racket[binding]. Unlike @racket[object-binding-fields], this
 does not include the name field.}

@defproc[(object-binding-private-accessors [binding object-binding?])
         (vectorof identifier? #:immutable #t)]{
 Returns a vector of identifiers that are bound at runtime to the per-field
 accessors of the object type bound by @racket[binding]. Unlike
 @racket[object-binding-field-accessors], this does not include an accessor for
 the name field.}

@defproc[(object-binding-name-field [binding object-binding?]) identifier?]{
 Returns an identifier for the name field of the object type bound by
 @racket[binding].}

@defproc[(object-binding-name-accessor [binding object-binding?]) identifier?]{
 Returns an identifier that is bound at runtime to the accessor or the name
 field of the object type bound by @racket[binding].}

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
