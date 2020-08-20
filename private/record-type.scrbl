#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/match
                     racket/math
                     rebellion/collection/keyset
                     rebellion/type/record
                     rebellion/type/record/binding
                     rebellion/type/struct
                     syntax/parse/define)
          (submod rebellion/private/scribble-cross-document-tech doc)
          (submod rebellion/private/scribble-evaluator-factory doc)
          (submod rebellion/private/scribble-index-attribute doc)
          scribble/examples)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'racket/match
                   'rebellion/type/record
                   'syntax/parse/define)
    #:private (list 'racket/base)))

@title{Record Types}
@defmodule[rebellion/type/record]

A @deftech{record type} is a kind of @tech{data type} for composite values that
contain an unordered set of named fields. The definition of each record type
declares how many fields it has and what their names are. Constructing an
instance of a record type requires passing a keyword argument for each field to
the type's constructor. Record types are useful when a fixed number of different
pieces of data together represent a single logical thing, and there isn't an
obvious order to those pieces.

@(examples
  #:eval (make-evaluator) #:once
  (eval:no-prompt
   (define-record-type opcode (name argument addressing-mode))
   (define add-42
     (opcode #:name 'ADD
             #:argument 42
             #:addressing-mode 'immediate)))

  add-42
  (opcode-name add-42))

@defform[
 (define-record-type id (field-id ...) option ...)

 #:grammar
 ([option
   #:omit-root-binding
   (code:line #:constructor-name constructor-id)
   (code:line #:predicate-name predicate-id)
   (code:line #:pattern-name pattern-id)
   (code:line #:property-maker prop-maker-expr)])
 
 #:contracts
 ([prop-maker-expr
   (-> uninitialized-record-descriptor?
       (listof (cons/c struct-type-property? any/c)))])]{
 Creates a new @tech{record type} named @racket[id] and binds the following
 identifiers:

 @itemlist[
 @item{@racket[constructor-id], which defaults to @racketidfont{
    constructor:}@racket[id] --- a constructor function that accepts one
   mandatory keyword argument for each @racket[field-id] and returns an instance
   of the created type.}

 @item{@racket[predicate-id], which defaults to @racket[id]@racketidfont{?} ---
   a predicate function that returns @racket[#t] when given instances of the
   created type and returns @racket[#f] otherwise.}

 @item{@racket[id]@racketidfont{-}@racket[field-id] for each @racket[field-id]
   --- an accessor function that returns the value for @racket[field-id] when
   given an instance of the created type.}

 @item{@racket[pattern-id], which defaults to @racketidfont{pattern:}@racket[id]
   --- a @tech/reference{match expander} that accepts one optional keyword and
   subpattern pair for each field and deconstructs instances of the created
   type, matching each field with its corresponding subpattern (if a subpattern
   for that field is given).}]

 Additionally, unless @racket[#:omit-root-binding] is specified, the original
 @racket[id] is bound to @racket[pattern-id] when used in match patterns and to
 @racket[constructor-id] when used as an expression. Use @racket[
 #:omit-root-binding] when you want control over what @racket[id] is bound to,
 such as when creating a smart constructor.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define-record-type color (red green blue))
    (define yellow  (color #:red 0 #:green 255 #:blue 255)))
   yellow
   (color? yellow)
   (color-red yellow)
   (color-green yellow)
   (color-blue yellow)
   (match yellow
     [(color #:red r #:blue b)
      (list r b)]))}

@section{Record Type Information}

@defproc[(record-type? [v any/c]) boolean?]{
 A predicate for @tech{record types}.}

@defproc[(record-type [name symbol?]
                      [fields keyset?]
                      [#:predicate-name predicate-name (or/c symbol? #f) #f]
                      [#:constructor-name constructor-name (or/c symbol? #f) #f]
                      [#:accessor-name accessor-name (or/c symbol? #f) #f])
         record-type?]{
 Constructs a @tech{record type} named @racket[name] with @racket[fields] as its
 field names. The optional @racket[predicate-name], @racket[constructor-name],
 and @racket[accessor-name] arguments control the result of @racket[object-name]
 on the functions implementing the type. They default to
 @racket[name]@racketidfont{?}, @racketidfont{constructor:}@racket[name], and
 @racketidfont{accessor:}@racket[name] respectively. This function only
 constructs the information representing a record type; to implement the type,
 use @racket[make-record-implementation].}

@deftogether[[
 @defproc[(record-type-name [type record-type?]) symbol?]
 @defproc[(record-type-fields [type record-type?]) keyset?]
 @defproc[(record-type-predicate-name [type record-type?]) (or/c symbol? #f)]
 @defproc[(record-type-constructor-name [type record-type?]) (or/c symbol? #f)]
 @defproc[(record-type-accessor-name [type record-type?]) (or/c symbol? #f)]]]{
 Accessors for the various components of a @tech{record type}.}

@section{Record Type Descriptors}

Record types are implemented using structs, where the fields of the struct are
always sorted by name. The @tech{type descriptor} for a @tech{record type}
contains two functions that implement the type:

@itemlist[
 @item{A @deftech{record constructor} that accepts a mandatory keyword argument
  for each field of the record type and constructs a record instance.}

 @item{A @deftech{record accessor} that accepts an instance of the record type
  and an integer field index, then returns the value of the corresponding field.
  Record fields are always sorted alphabetically, and the field index
  corresponding to a given field name can be retrieved from the record type
  using the @racket[record-type-fields] @tech{keyset}.}]

These functions can be used to dynamically construct and inspect instances of
arbitrary record types at runtime, assuming their record descriptor is
initialized. Note that the descriptor contains a single accessor function that
can access any field in the record: the per-field accessors created by
@racket[define-record-type] are merely convenient wrappers around this accessor.

@defproc[(record-descriptor? [v any/c]) boolean?]{
 A predicate for record @tech{type descriptors}.}

@defproc[(initialized-record-descriptor? [v any/c]) boolean?]{
 A predicate for initialized record @tech{type descriptors}.}

@defproc[(uninitialized-record-descriptor? [v any/c]) boolean?]{
 A predicate for uninitialized record @tech{type descriptors}.}

@defproc[(make-record-implementation
          [type record-type?]
          [#:inspector inspector inspector? (current-inspector)]
          [#:property-maker prop-maker
           (-> uninitialized-record-descriptor?
               (listof (cons/c struct-type-property? any/c)))
           default-record-properties])
         initialized-record-descriptor?]{
 Implements @racket[type] and returns a @tech{type descriptor} for the new
 implementation. The @racket[inspector] argument behaves the same as in
 @racket[make-struct-type], although there are no transparent or prefab record
 types. The @racket[prop-maker] argument is similar to the corresponding
 argument of @racket[make-struct-implementation]. By default, record types are
 created with properties that make them print like transparent structures,
 except field names are included --- see @racket[default-record-properties] for
 details.}

@defproc[(record-descriptor-type [descriptor record-descriptor?]) record-type?]{
 Returns the @tech{record type} that @racket[descriptor] implements.}

@defproc[(record-descriptor-predicate [descriptor record-descriptor?])
         (-> any/c boolean?)]{
 Returns a predicate that returns true when given any record instance created by
 @racket[descriptor]. The predicate is specific to @racket[descriptor] --- it
 will not return true for record instances created by any other record
 descriptors, even if they're different implementations of the same record type
 as @racket[descriptor]. This is because record types are @tech{nominal types}.}

@defproc[(record-descriptor-constructor [descriptor record-descriptor?])
         procedure?]{
 Returns the @tech{record constructor} of the record type implementation
 represented by @racket[descriptor]. The constructor accepts one mandatory
 keyword argument for each field in the record type and returns an instance of
 the record type.}

@defproc[(record-descriptor-accessor [descriptor record-descriptor?])
         (-> (record-descriptor-predicate descriptor) natural? any/c)]{
 Returns the @tech{record accessor} of the record type implementation
 represented by @racket[descriptor]. The accessor accepts two arguments: a
 record instance created by @racket[descriptor], and a nonnegative integer less
 than the number of fields in the record type. The accessor returns the value of
 the corresponding field in the instance. To access fields by name, first
 determine the integer index of the field name using the @tech{keyset} returned
 by @racket[record-type-fields]. See also @racket[make-record-field-accessor] to
 construct a field-specific accessor function.}

@defproc[(make-record-field-accessor [descriptor record-descriptor?]
                                     [field natural?])
         (-> (record-descriptor-predicate descriptor) any/c)]{
 Constructs a function that accepts an instance of the record type implemented
 by @racket[descriptor] and returns the value of the field at position
 @racket[field] in the record.}

@defproc[(default-record-properties [descriptor record-descriptor?])
         (listof (cons/c struct-type-property? any/c))]{
 Returns implementations of @racket[prop:equal+hash] and
 @racket[prop:custom-write] suitable for most @tech{record types}. This function
 is called by @racket[make-record-implementation] when no @racket[_prop-maker]
 argument is supplied.}

@defproc[(default-record-equal+hash [descriptor record-descriptor?])
         equal+hash/c]{
 Builds an equality-checking function, a hashing function, and a secondary
 hashing function suitable for use with @racket[prop:equal+hash], each of which
 operate on record instances created by @racket[descriptor]. Two instances are
 equal if and only if the fields of one instance are equal to the fields of the
 other. This function is used by @racket[default-record-properties] to implement
 @racket[prop:equal+hash].}

@defproc[(default-record-custom-write [descriptor record-descriptor?])
         custom-write-function/c]{
 Builds a @tech{custom write implementation} that prints record instances
 created by @racket[descriptor] in a manner similar to transparent Racket
 structs, except field values are prefixed by field names. Field names are
 printed as keywords, and fields are always printed in alphabetic order by name.
 Pretty printing is supported: the pretty printer will insert linebreaks between
 fields if needed, and it will keep field names and field values on the same
 line. This function is used by @racket[default-record-properties] to implement
 @racket[prop:custom-write].}

@section{Record Type Bindings}
@defmodule[rebellion/type/record/binding]

An @deftech{record type binding} is a @tech{type binding} for
@tech{record types}. Record type bindings contain compile-time information about
the record type's name and fields, as well as runtime bindings for its
predicate, @tech{type descriptor}, and other runtime components. To extract an
record type binding bound by @racket[define-record-type], use the
@racket[record-id] @syntax-tech{syntax class}.

@defproc[(record-binding? [v any/c]) boolean?]{
 A predicate for @tech{record type bindings}.}

@defidform[#:kind "syntax class" record-id]{
 A @syntax-tech{syntax class} for @tech{record type bindings} bound by
 @racket[define-record-type]. This class matches any @tech/reference{identifier}
 bound with @racket[define-syntax] to a value satisfying the
 @racket[record-binding?] predicate, similar to the @racket[static] syntax
 class. Upon a successful match, the @racket[record-id] class defines the
 following attributes:

 @itemlist[

 @item{@index-attribute[record-id type] --- an attribute bound to a compile-time
   @racket[record-type?] value describing the type.}

 @item{@index-attribute[record-id name] --- a pattern variable bound to the
   record type's name, as a quoted symbol.}

 @item{@index-attribute[record-id field-name ...] --- a pattern variable bound
   to the record's field names, as quoted symbols.}

 @item{@index-attribute[record-id descriptor] --- a pattern variable bound to
   the record type's runtime @tech{type descriptor}.}

 @item{@index-attribute[record-id predicate] --- a pattern variable bound to the
   record type's runtime type predicate.}

 @item{@index-attribute[record-id constructor] --- a pattern variable bound to
   the record type's runtime @tech{record constructor}.}

 @item{@index-attribute[record-id accessor] --- a pattern variable bound to the
   record type's runtime @tech{record accessor}.}

 @item{@index-attribute[record-id field-accessor ...] --- a pattern variable
   bound to the record type's per-field runtime field accessors.}]

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (require (for-syntax rebellion/type/record/binding))
    
    (define-simple-macro (record-field-names record:record-id)
      (list record.field-name ...))

    (define-record-type email (subject author body))

    (record-field-names email)))}

@defproc[(record-binding-type [binding record-binding?]) record-type?]{
 Returns the @tech{record type} that @racket[binding] is for. When a record type
 binding is bound with @racket[define-syntax], this can be used at compile-time
 to obtain information about the name and fields of the record type.}

@defproc[(record-binding-descriptor [binding record-binding?]) identifier?]{
 Returns an identifier that is bound at runtime to the @tech{type descriptor}
 for the record type bound by @racket[binding]. When a record type binding is
 bound with @racket[define-syntax], this can be used in macro-generated code to
 work with record types dynamically.}

@defproc[(record-binding-predicate [binding record-binding?]) identifier?]{
 Returns an identifier that is bound at runtime to the predicate for the record
 type bound by @racket[binding].}

@defproc[(record-binding-constructor [binding record-binding?]) identifier?]{
 Returns an identifier that is bound at runtime to the @tech{record constructor}
 for the record type bound by @racket[binding].}

@defproc[(record-binding-accessor [binding record-binding?]) identifier?]{
 Returns an identifier that is bound at runtime to the @tech{record accessor}
 for the record type bound by @racket[binding].}

@defproc[(record-binding-field-accessors [binding record-binding?])
         (vectorof identifier #:immutable #t)]{
 Returns a vector of identifiers that are bound at runtime to the per-field
 accessors of the record type bound by @racket[binding].}
