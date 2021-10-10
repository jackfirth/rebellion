#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/contract/region
                     racket/contract/combinator
                     racket/math
                     rebellion/base/symbol
                     rebellion/custom-write
                     rebellion/equal+hash
                     rebellion/type/singleton
                     rebellion/type/singleton/binding
                     syntax/parse/define)
          (submod rebellion/private/scribble-cross-document-tech doc)
          (submod rebellion/private/scribble-evaluator-factory doc)
          (submod rebellion/private/scribble-index-attribute doc)
          scribble/examples)

@(define reference-path '(lib "scribblings/reference/reference.scrbl"))

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'racket/contract/base
                   'racket/contract/region
                   'racket/contract/combinator
                   'rebellion/type/singleton
                   'syntax/parse/define)
    #:private (list 'racket/base)))

@title{Singleton Types}
@defmodule[rebellion/type/singleton]

A @deftech{singleton type} is a simple kind of @tech{data type} made up of only
one value. Singleton types are useful for representing named constants, such as
the end-of-file marker value @racket[eof]. Symbols can also be used for this
purpose, but a singleton type is often preferable because misspelling the name
of the constant causes a compile error rather than a silent bug at runtime.

@(examples
  #:eval (make-evaluator) #:once
  (eval:no-prompt
   (define-singleton-type undefined)

   (define/contract (divide x y)
     (-> real? real? (or/c real? undefined?))
     (if (zero? y)
         undefined
         (/ x y))))

  (divide 6 3)
  (divide 6 0))

@defform[
 (define-singleton-type id singleton-option ...)
 #:grammar
 ([singleton-option
   #:omit-root-binding
   (code:line #:descriptor-name descriptor-id)
   (code:line #:predicate-name predicate-id)
   (code:line #:instance-name instance-id)
   (code:line #:property-maker prop-maker-expr)
   (code:line #:inspector inspector-expr)])
 #:contracts
 ([prop-maker-expr
   (-> uninitialized-singleton-descriptor?
       (listof (cons/c struct-type-property? any/c)))]
  [inspector-expr inspector?])]{
 Creates a @tech{singleton type} and binds the following identifiers:

 @itemlist[
 @item{@racket[instance-id], which defaults to
   @racketidfont{instance:}@racket[id] --- the unique instance of the created
   type.}

 @item{@racket[predicate-id], which defaults to @racket[id]@racketidfont{?} ---
   a predicate that returns @racket[#t] when given the singleton instance and
   false for all other values.}
 
 @item{@racket[descriptor-id], which defaults to
   @racketidfont{descriptor:}@racket[id] --- the @tech{type descriptor} for the
   created type.}]

  Additionally, unless @racket[#:omit-root-binding] is specified, the original
 @racket[id] is bound to a @tech{singleton type binding} for the created type.
 The binding behaves like @racket[instance-id] when used as an expression.
 
 The @racket[prop-maker-expr] is used to add structure type properties to the
 created type, and @racket[inspector-expr] is used to determine the
 @tech/reference{inspector} that will control the created type. See
 @racket[make-singleton-implementation] for more information about these
 parameters.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt (define-singleton-type infinity))

   infinity
   (infinity? infinity))}

@defform[#:kind "provide transformer" (singleton-out singleton)]{
 Provides @racket[singleton], which must be a @racket[singleton-id], along with
 its predicate.}

@section{Singleton Type Information}

@defproc[(singleton-type? [v any/c]) boolean?]{
 A predicate for @tech{singleton types}.}

@defproc[(singleton-type
          [name interned-symbol?]
          [#:predicate-name predicate-name (or/c interned-symbol? #f) #f])
         singleton-type?]{
 Constructs a @tech{singleton type} named @racket[name]. The optional
 @racket[predicate-name] argument controls the result of @racket[object-name] on
 a predicate implementing the type. It defaults to
 @racket[name]@racketidfont{?}. This function only constructs the information
 representing a singleton type; to implement the type, use
 @racket[make-singleton-implementation].}

@defproc[(singleton-type-name [type singleton-type?]) interned-symbol?]{
 Returns the name of @racket[type].}

@defproc[(singleton-type-predicate-name [type singleton-type?])
         interned-symbol?]{
 Returns the name that should be used for predicates implementing
 @racket[type].}

@section{Singleton Type Descriptors}

Singleton types are implemented using fieldless structs, where only a single
instance of the struct can be created. The @tech{type descriptor} for a
@tech{singleton type} provides a predicate and, if the descriptor is
initialized, can be used to retrieve the singleton instance.

@defproc[(singleton-descriptor? [v any/c]) boolean?]{
 A predicate for singleton @tech{type descriptors}.}

@defproc[(initialized-singleton-descriptor? [v any/c]) boolean?]{
 A predicate for initialized singleton @tech{type descriptors}.}

@defproc[(uninitialized-singleton-descriptor? [v any/c]) boolean?]{
 A predicate for uninitialized singleton @tech{type descriptors}.}

@defproc[(make-singleton-implementation
          [type singleton-type?]
          [#:inspector inspector inspector? (current-inspector)]
          [#:property-maker prop-maker
           (-> uninitialized-singleton-descriptor?
               (listof (cons/c struct-type-property? any/c)))
           default-singleton-properties])
         initialized-singleton-descriptor?]{
 Implements @racket[type] and returns a @tech{type descriptor} for the new
 implementation. The @racket[inspector] argument behaves the same as in
 @racket[make-struct-type], although there are no transparent or prefab
 singleton types. The @racket[prop-maker] argument is similar to the
 corresponding argument of @racket[make-struct-implementation]. By default,
 singleton types are created with properties that make them print like other
 named Racket constants --- see @racket[default-singleton-properties] for
 details.}

@defproc[(singleton-descriptor-type [descriptor singleton-descriptor?])
         singleton-type?]{
 Returns the @tech{singleton type} that @racket[descriptor] implements.}

@defproc[(singleton-descriptor-predicate [descriptor singleton-descriptor?])
         predicate/c]{
 Returns a predicate that returns true when given the singleton instance created
 by @racket[descriptor]. The predicate is specific to @racket[descriptor] --- it
 will not return true for singleton instances created by any other singleton
 descriptors, even if they're different implementations of the same singleton
 type as @racket[descriptor]. This is because singleton types are
 @tech{nominal types}.}

@defproc[(singleton-descriptor-instance
          [descriptor initialized-singleton-descriptor?])
         (singleton-descriptor-predicate descriptor)]{
 Returns the instance of the singleton type implemented by @racket[descriptor].}

@defproc[(default-singleton-properties [descriptor singleton-descriptor?])
         (listof (cons/c struct-type-property? any/c))]{
 Returns implementations of @racket[prop:equal+hash], @racket[prop:custom-write],
 @racket[prop:object-name], and @racket[prop:flat-contract] suitable for most
 @tech{singleton types}. The default implementation of @racket[prop:equal+hash]
 always returns true, since it's only called if two values are instances of the
 same type and singleton types only have one value.

 This function is called by @racket[make-singleton-implementation] when no
 @racket[_prop-maker] argument is supplied.}

@defproc[(default-singleton-custom-write [descriptor singleton-descriptor?])
         custom-write-function/c]{
 Builds a @tech{custom write implementation} that prints singleton instances
 created by @racket[descriptor] in a manner similar to other named Racket
 constants such as @racket[eof]. This function is used by
 @racket[default-singleton-properties] to implement @racket[prop:custom-write].}

@defproc[(default-singleton-object-name [descriptor singleton-descriptor?])
         (or/c natural? (-> any/c any/c))]{
 Builds a value suitable for use with @racket[prop:object-name] that causes
 @racket[object-name] to return the name of the instance when used on the
 singleton instance created by @racket[descriptor]. This function is used by
 @racket[default-singleton-properties] to implement @racket[prop:object-name].}

@defproc[(default-singleton-flat-contract [descriptor singleton-descriptor?])
         flat-contract-property?]{
 Builds a @tech[#:doc reference-path]{flat contract property} suitable for
 use with @racket[prop:flat-contract]. The contract accepts a value if and
 only if it is the singleton instance. This function is used by
 @racket[default-singleton-properties] to implement @racket[prop:flat-contract].}

@section{Singleton Type Bindings}
@defmodule[rebellion/type/singleton/binding]

A @deftech{singleton type binding} is a @tech{type binding} for a
@tech{singleton type}. Singleton type bindings contain compile-time information
about the singleton type's name, as well as runtime bindings for its predicate,
@tech{type descriptor}, and instance. To extract a singleton type binding bound
by @racket[define-singleton-type], use the @racket[singleton-id]
@syntax-tech{syntax class}.

@defproc[(singleton-binding? [v any/c]) boolean?]{
 A predicate for @tech{singleton type bindings}.}

@defidform[#:kind "syntax class" singleton-id]{
 A @syntax-tech{syntax class} for @tech{singleton type bindings} bound by
 @racket[define-singleton-type]. This class matches any
 @tech/reference{identifier} bound with @racket[define-syntax] to a value
 satisfying the @racket[singleton-binding?] predicate, similar to the
 @racket[static] syntax class. Upon a successful match, the
 @racket[singleton-id] class defines the following attributes:

 @itemlist[

 @item{@index-attribute[singleton-id type] --- an attribute bound to a
   compile-time @racket[singleton-type?] value describing the type.}

 @item{@index-attribute[singleton-id binding] --- an attribute bound to the
   compile-time @racket[singleton-binding?] value extracted from the matched
   identifier.}

 @item{@index-attribute[singleton-id name] --- a pattern variable bound to the
   singleton type's name, as a quoted symbol.}

 @item{@index-attribute[singleton-id descriptor] --- a pattern variable bound to
   the singleton type's runtime @tech{type descriptor}.}

 @item{@index-attribute[singleton-id predicate] --- a pattern variable bound to
   the singleton type's runtime type predicate.}

 @item{@index-attribute[singleton-id instance] --- a pattern variable bound to
   the singleton type's runtime singleton instance.}]

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (require (for-syntax rebellion/type/singleton/binding))
    
    (define-simple-macro (singleton-predicate singleton:singleton-id)
      singleton.predicate)

    (define-singleton-type null))

   (singleton-predicate null))}

@defproc[(singleton-binding-type [binding singleton-binding?]) singleton-type?]{
 Returns the @tech{singleton type} that @racket[binding] is for. When a
 singleton type binding is bound with @racket[define-syntax], this can be used
 at compile-time to obtain information about the name of the singleton type.}

@defproc[(singleton-binding-descriptor [binding singleton-binding?])
         identifier?]{
 Returns an identifier that is bound at runtime to the @tech{type descriptor}
 for the singleton type bound by @racket[binding]. When a singleton type binding
 is bound with @racket[define-syntax], this can be used in macro-generated code
 to work with singleton types dynamically.}

@defproc[(singleton-binding-predicate [binding singleton-binding?])
         identifier?]{
 Returns an identifier that is bound at runtime to the predicate for the
 singleton type bound by @racket[binding].}

@defproc[(singleton-binding-instance [binding singleton-binding?]) identifier?]{
 Returns an identifier that is bound at runtime to the singleton instance for
 the singleton type bound by @racket[binding].}
