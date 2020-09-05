#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/contract/region
                     racket/match
                     racket/math
                     racket/pretty
                     racket/sequence
                     racket/struct
                     rebellion/base/symbol
                     rebellion/custom-write
                     rebellion/equal+hash
                     rebellion/type/tuple
                     rebellion/type/tuple/binding
                     rebellion/type/struct
                     syntax/parse/define)
          (submod rebellion/private/scribble-cross-document-tech doc)
          (submod rebellion/private/scribble-evaluator-factory doc)
          (submod rebellion/private/scribble-index-attribute doc)
          scribble/examples)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'racket/contract/base
                   'racket/contract/region
                   'racket/match
                   'racket/math
                   'racket/pretty
                   'rebellion/type/tuple
                   'syntax/parse/define)
    #:private (list 'racket/base)))

@title{Tuple Types}
@defmodule[rebellion/type/tuple]

A @deftech{tuple type} is a kind of @tech{data type} for composite values that
contain an ordered list of fields. The definition of each tuple type declares
how many fields it has and what their names are, although those names are only
observable at compile time. Constructing an instance of a tuple type requires
passing a positional argument for each field to the type's constructor. Tuple
types are useful when a fixed number of different pieces of data together
represent a single logical thing, and there is an obvious order to those pieces.

@(examples
  #:eval (make-evaluator) #:once
  (eval:no-prompt
   (define-tuple-type point (x y))

   (define/contract (point-distance start end)
     (-> point? point? real?)
     (match-define (point x1 y1) start)
     (match-define (point x2 y2) end)
     (define dx (- x2 x1))
     (define dy (- y2 y1))
     (sqrt (+ (sqr dx) (sqr dy)))))

  (point-distance (point 0 0) (point 3 4))
  (eval:error (point-distance (point 0 0) (list 3 4))))

@defform[
 (define-tuple-type id (field-id ...) option ...)

 #:grammar
 ([option
   #:omit-root-binding
   (code:line #:descriptor-name descriptor-id)
   (code:line #:predicate-name predicate-id)
   (code:line #:constructor-name constructor-id)
   (code:line #:accessor-name accessor-id)
   (code:line #:pattern-name pattern-id)
   (code:line #:property-maker prop-maker-expr)
   (code:line #:inspector inspector-expr)])

 #:contracts
 ([prop-maker-expr
   (-> uninitialized-tuple-descriptor?
       (listof (cons/c struct-type-property? any/c)))]
  [inspector-expr inspector?])]{
 Creates a new @tech{tuple type} named @racket[id] and binds the following
 identifiers:

 @itemlist[
 @item{@racket[constructor-id], which defaults to
   @racketidfont{constructor:}@racket[id] --- a @tech{tuple constructor} that
   accepts one positional argument for each @racket[field-id] and returns an
   instance of the created type.}

 @item{@racket[predicate-id], which defaults to @racket[id]@racketidfont{?} ---
   a predicate function that returns @racket[#t] when given instances of the
   created type and returns @racket[#f] otherwise.}

 @item{@racket[id]@racketidfont{-}@racket[field-id] for each @racket[field-id]
   --- a field accessor function that returns the value for @racket[field-id]
   when given an instance of the created type.}

 @item{@racket[accessor-id], which defaults @racketidfont{accessor:}@racket[id]
   --- a @tech{tuple accessor} that accepts an instance of the created type and
   an integer indicating which field to access, then returns the value of that
   field.}

 @item{@racket[descriptor-id], which defaults to
   @racketidfont{descriptor:}@racket[id] --- the @tech{type descriptor} for the
   created type.}

 @item{@racket[pattern-id], which defaults to @racketidfont{pattern:}@racket[id]
   --- a @tech/reference{match expander} that deconstructs instances of the
   created type and matches each field against a subpattern}]

 Additionally, unless @racket[#:omit-root-binding] is specified, the original
 @racket[id] is bound to a @tech{tuple type binding} for the created type. The
 binding behaves like @racket[pattern-id] when used in match patterns and like
 @racket[constructor-id] when used as an expression. Use @racket[
 #:omit-root-binding] when you want control over what @racket[id] is bound to,
 such as when creating a smart constructor.

 The @racket[prop-maker-expr] is used to add structure type properties to the
 created type, and @racket[inspector-expr] is used to determine the
 @tech/reference{inspector} that will control the created type. See
 @racket[make-tuple-implementation] for more information about these parameters.
 
 @(examples
   #:eval (make-evaluator) #:once
   (define-tuple-type point (x y))
   (point 1 2)
   (point? (point 1 2))
   (point-x (point 42 0))
   (point-y (point 42 0))
   (match-define (point (? positive? x) (? negative? y)) (point 3 -3)))}

@section{Tuple Type Information}

@defproc[(tuple-type? [v any/c]) boolean?]{
 A predicate for @tech{tuple types}.}

@defproc[
 (tuple-type [name interned-symbol?]
             [fields (sequence/c interned-symbol?)]
             [#:predicate-name predicate-name (or/c interned-symbol? #f) #f]
             [#:constructor-name constructor-name (or/c interned-symbol? #f) #f]
             [#:accessor-name accessor-name (or/c interned-symbol? #f) #f])
 tuple-type?]{
 Constructs a @tech{tuple type} named @racket[name] and with fields named
 @racket[fields]. The optional @racket[predicate-name],
 @racket[constructor-name], and @racket[accessor-name] arguments control the
 result of @racket[object-name] on the functions implementing the type. If not
 provided, @racket[predicate-name] defaults to @racket[name]@racketidfont{?},
 @racket[constructor-name] defaults to @racketidfont{constructor:}@racket[name],
 and @racket[accessor-name] defaults to @racketidfont{accessor:}@racket[name].
 Two tuple types constructed with the same arguments are @racket[equal?]. To
 make an implementation of a tuple type, see
 @racket[make-tuple-implementation].}

@deftogether[[
 @defproc[(tuple-type-name [type tuple-type?]) interned-symbol?]
 @defproc[(tuple-type-fields [type tuple-type?])
          (vectorof interned-symbol? #:immutable #t)]
 @defproc[(tuple-type-predicate-name [type tuple-type?]) interned-symbol?]
 @defproc[(tuple-type-constructor-name [type tuple-type?]) interned-symbol?]
 @defproc[(tuple-type-accessor-name [type tuple-type?]) interned-symbol?]]]{
 Accessors for the various fields of a @tech{tuple type}.}

@defproc[(tuple-type-size [type tuple-type?]) natural?]{
 Returns the number of fields in @racket[type].}

@section{Tuple Type Descriptors}

The @tech{type descriptor} for a @tech{tuple type} contains two functions that
implement the type:

@itemlist[
 @item{A @deftech{tuple constructor} that accepts one argument for each field of
  the tuple type and constructs a tuple instance.}

 @item{A @deftech{tuple accessor} that accepts an instance of the tuple type
  and an integer field index, then returns the value of the corresponding
  field.}]

These functions can be used to dynamically construct and inspect instances of
arbitrary tuple types at runtime, assuming the type's descriptor is initialized.
Note that the descriptor contains a single accessor function that can access any
field in the tuple: the per-field accessors created by
@racket[define-tuple-type] are merely convenient wrappers around this accessor.

@defproc[(tuple-descriptor? [v any/c]) boolean?]{
 A predicate for @tech{type descriptors} of @tech{tuple types}.}

@deftogether[[
 @defproc[(initialized-tuple-descriptor? [v any/c]) boolean?]
 @defproc[(uninitialized-tuple-descriptor? [v any/c]) boolean?]]]{
 Predicates for tuple @tech{type descriptors} that either have or haven't been
 initialized yet. The predicate, constructor, and accessor of an uninitialized
 tuple descriptor @bold{must not be called until initialization is complete}, as
 the implementations of those functions won't exist until then. Initialization
 of a descriptor completes when @racket[make-tuple-implementation]
 returns.}

@defproc[
 (make-tuple-implementation
  [type tuple-type?]
  [#:guard guard (or/c procedure? #f) #f]
  [#:inspector inspector inspector? (current-inspector)]
  [#:property-maker prop-maker
   (-> uninitialized-tuple-descriptor?
       (listof (cons/c struct-type-property? any/c)))
   default-tuple-properties])
 initialized-tuple-descriptor?]{
 Implements @racket[type] and returns a @tech{type descriptor} for the new
 implementation. The @racket[guard] and @racket[inspector] arguments behave the
 same as the corresponding arguments of @racket[make-struct-type], although
 there are no transparent or prefab tuple types. The @racket[prop-maker]
 argument is similar to the corresponding argument of @racket[
 make-struct-implementation]. By default, tuple types are created with
 properties that make them print and compare in a manner similar to transparent
 structure types --- see @racket[default-tuple-properties] for details.

 @(examples
   #:eval (make-evaluator) #:once
   (define point-descriptor
     (make-tuple-implementation (tuple-type 'point (list 'x 'y))))
   (define point (tuple-descriptor-constructor point-descriptor))
   (define point-x (make-tuple-field-accessor point-descriptor 0))
   (define point-y (make-tuple-field-accessor point-descriptor 1))
   (point 42 888)
   (point-x (point 42 888))
   (point-y (point 42 888)))}

@defproc[(tuple-descriptor-type [descriptor tuple-descriptor?]) tuple-type?]{
 Returns the @tech{tuple type} that @racket[descriptor] implements.}

@defproc[(tuple-descriptor-predicate [descriptor tuple-descriptor?])
         (-> any/c boolean?)]{
 Returns a predicate that returns true when given any tuple instance created by
 @racket[descriptor]. The predicate is specific to @racket[descriptor] --- it
 will not return true for tuple instances created by any other tuple
 descriptors, even if they're different implementations of the same tuple type
 as @racket[descriptor]. This is because tuple types are @tech{nominal types}.}

@defproc[(tuple-descriptor-constructor [descriptor tuple-descriptor?])
         procedure?]{
 Returns the @tech{tuple constructor} of the tuple type implementation
 represented by @racket[descriptor]. The constructor accepts one argument for
 each field in the tuple type and returns an instance of the tuple type.}

@defproc[(tuple-descriptor-accessor [descriptor tuple-descriptor?])
         (-> (tuple-descriptor-predicate descriptor) natural? any/c)]{
 Returns the @tech{tuple accessor} of the tuple type implementation
 represented by @racket[descriptor]. The accessor accepts two arguments: a
 tuple instance created by @racket[descriptor], and a nonnegative integer less
 than the number of fields in the tuple type. The accessor returns the value of
 the corresponding field in the instance. See also
 @racket[make-tuple-field-accessor] to construct a field-specific accessor
 function.}

@defproc[
 (make-tuple-field-accessor [descriptor tuple-descriptor?]
                            [pos natural?])
 (-> (tuple-descriptor-predicate descriptor) any/c)]{
 Builds a field accessor function that returns the @racket[pos] field of
 instances of the @tech{tuple type} implemented by @racket[descriptor]. See
 @racket[make-tuple-implementation] for usage examples.}

@defproc[(default-tuple-properties [descriptor tuple-descriptor?])
         (listof (cons/c struct-type-property? any/c))]{
 Returns implementations of @racket[prop:equal+hash] and @racket[
 prop:custom-write] suitable for most @tech{tuple types}. This function is
 called by @racket[make-tuple-implementation] when no @racket[_prop-maker]
 argument is supplied.}

@defproc[(default-tuple-equal+hash [descriptor tuple-descriptor?])
         equal+hash/c]{
 Builds an equality-checking function, a hashing function, and a secondary
 hashing function suitable for use with @racket[prop:equal+hash], each of which
 operate on instances of @racket[descriptor]. All fields in @racket[descriptor]
 are compared and hashed by the returned procedures. This causes @racket[equal?]
 to behave roughly the same as it does on transparent structure types. This
 function is used by @racket[default-tuple-properties] to implement
 @racket[prop:equal+hash].

 @(examples
   #:eval (make-evaluator) #:once
   (define-tuple-type point (x y)
     #:property-maker
     (λ (descriptor)
       (list (cons prop:equal+hash (default-tuple-equal+hash descriptor)))))
   (equal? (point 1 2) (point 1 2))
   (equal? (point 1 2) (point 2 1)))}

@defproc[(default-tuple-custom-write [descriptor tuple-descriptor?])
         custom-write-function/c]{
 Constructs a @tech{custom write implementation} that prints instances of the
 @tech{tuple type} described by @racket[descriptor] in a manner similar to the
 way that @racket[make-constructor-style-printer] prints values. This function
 is used by @racket[default-tuple-properties] to implement
 @racket[prop:custom-write].

 @(examples
   #:eval (make-evaluator) #:once
   (define-tuple-type point (x y)
     #:property-maker
     (λ (descriptor)
       (define custom-write (default-tuple-custom-write descriptor))
       (list (cons prop:custom-write custom-write))))

   (point 1 2)
   (parameterize ([pretty-print-columns 10])
     (pretty-print (point 100000000000000 200000000000000))))}

@section{Tuple Type Bindings}
@defmodule[rebellion/type/tuple/binding]

A @deftech{tuple type binding} is a @tech{type binding} for a @tech{tuple type}.
Tuple type bindings contain compile-time information about the tuple type's name
and fields, as well as runtime bindings for its predicate,
@tech{type descriptor}, and other runtime components. To extract a tuple type
binding bound by @racket[define-tuple-type], use the @racket[tuple-id]
@syntax-tech{syntax class}.

@defproc[(tuple-binding? [v any/c]) boolean?]{
 A predicate for @tech{tuple type bindings}.}

@defidform[#:kind "syntax class" tuple-id]{
 A @syntax-tech{syntax class} for @tech{tuple type bindings} bound by
 @racket[define-tuple-type]. This class matches any @tech/reference{identifier}
 bound with @racket[define-syntax] to a value satisfying the
 @racket[tuple-binding?] predicate, similar to the @racket[static] syntax
 class. Upon a successful match, the @racket[tuple-id] class defines the
 following attributes:
 @itemlist[

 @item{@index-attribute[tuple-id type] --- an attribute bound to a compile-time
   @racket[tuple-type?] value describing the type.}

 @item{@index-attribute[tuple-id binding] --- an attribute bound to the
   compile-time @racket[tuple-binding?] value extracted from the matched
   identifier.}

 @item{@index-attribute[tuple-id name] --- a pattern variable bound to the tuple
   type's name, as a quoted symbol.}

 @item{@index-attribute[tuple-id descriptor] --- a pattern variable bound to the
   tuple type's runtime @tech{type descriptor}.}

 @item{@index-attribute[tuple-id predicate] --- a pattern variable bound to the
   tuple type's runtime type predicate.}

 @item{@index-attribute[tuple-id constructor] --- a pattern variable bound to
   the tuple type's runtime @tech{tuple constructor}.}

 @item{@index-attribute[tuple-id accessor] --- a pattern variable bound to the
   tuple type's runtime @tech{tuple accessor}.}

 @item{@index-attribute[tuple-id field ...] --- a pattern variable bound to
   the tuple's field identifiers.}

 @item{@index-attribute[tuple-id field-name ...] --- a pattern variable bound to
   the tuple's field names, as quoted symbols.}

 @item{@index-attribute[tuple-id field-accessor ...] --- a pattern variable
   bound to the tuple type's per-field runtime field accessors.}]

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (require (for-syntax rebellion/type/tuple/binding))
    
    (define-simple-macro (tuple-field-names tuple:tuple-id)
      (list tuple.field-name ...))

    (define-tuple-type point (x y z))

    (tuple-field-names point)))}

@defproc[(tuple-binding-type [binding tuple-binding?]) tuple-type?]{
 Returns the @tech{tuple type} that @racket[binding] is for. When a tuple type
 binding is bound with @racket[define-syntax], this can be used at compile-time
 to obtain information about the name and fields of the tuple type.}

@defproc[(tuple-binding-descriptor [binding tuple-binding?]) identifier?]{
 Returns an identifier that is bound at runtime to the @tech{type descriptor}
 for the tuple type bound by @racket[binding]. When a tuple type binding is
 bound with @racket[define-syntax], this can be used in macro-generated code to
 work with tuple types dynamically.}

@defproc[(tuple-binding-predicate [binding tuple-binding?]) identifier?]{
 Returns an identifier that is bound at runtime to the predicate for the tuple
 type bound by @racket[binding].}

@defproc[(tuple-binding-constructor [binding tuple-binding?]) identifier?]{
 Returns an identifier that is bound at runtime to the @tech{tuple constructor}
 for the tuple type bound by @racket[binding].}

@defproc[(tuple-binding-accessor [binding tuple-binding?]) identifier?]{
 Returns an identifier that is bound at runtime to the @tech{tuple accessor}
 for the tuple type bound by @racket[binding].}

@defproc[(tuple-binding-fields [binding tuple-binding?])
         (vectorof identifier? #:immutable #t)]{
 Returns a vector of the field identifiers for the tuple type bound by
 @racket[binding].}

@defproc[(tuple-binding-field-accessors [binding tuple-binding?])
         (vectorof identifier? #:immutable #t)]{
 Returns a vector of identifiers that are bound at runtime to the per-field
 accessors of the tuple type bound by @racket[binding].}

@section{Tuple Chaperones and Impersonators}

@defproc[(tuple-impersonate
          [instance (tuple-descriptor-predicate descriptor)]
          [descriptor initialized-tuple-descriptor?]
          [#:properties properties
           (hash/c impersonator-property? any/c #:immutable #t)
           empty-hash]
          [#:chaperone? chaperone? boolean? #t])
         (tuple-descriptor-predicate descriptor)]{
 Returns an @tech/reference{impersonator} of @racket[instance] with each
 @tech/reference{impersonator property} in @racket[properties] attached to it.
 If @racket[chaperone?] is true (the default), the returned impersonator is a
 @tech/reference{chaperone}.}
