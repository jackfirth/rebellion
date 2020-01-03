#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/contract/region
                     racket/match
                     racket/math
                     racket/pretty
                     racket/struct
                     rebellion/custom-write
                     rebellion/equal+hash
                     rebellion/type/tuple
                     rebellion/type/struct)
          (submod rebellion/private/scribble-cross-document-tech doc)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/examples)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'racket/contract/base
                   'racket/contract/region
                   'racket/match
                   'racket/math
                   'racket/pretty
                   'rebellion/type/tuple)
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
   (code:line #:constructor-name constructor-id)
   (code:line #:predicate-name predicate-id)
   (code:line #:pattern-name pattern-id)
   (code:line #:property-maker prop-maker-expr)])

 #:contracts
 ([prop-maker-expr
   (-> uninitialized-tuple-descriptor?
       (listof (cons/c struct-type-property? any/c)))])]{
 Creates a new @tech{tuple type} named @racket[id] and binds the following
 identifiers:

 @itemlist[
 @item{@racket[constructor-id], which defaults to @racketidfont{
    constructor:}@racket[id] --- a constructor function that accepts one
   positional argument for each @racket[field-id] and returns an instance of the
   created type.}

 @item{@racket[predicate-id], which defaults to @racket[id]@racketidfont{?} ---
   a predicate function that returns @racket[#t] when given instances of the
   created type and returns @racket[#f] otherwise.}

 @item{@racket[id]@racketidfont{-}@racket[field-id] for each @racket[field-id]
   --- an accessor function that returns the value for @racket[field-id] when
   given an instance of the created type.}

 @item{@racket[pattern-id], which defaults to @racketidfont{pattern:}@racket[id]
   --- a @tech/reference{match expander} that deconstructs instances of the
   created type and matches each field against a subpattern}]

 Additionally, unless @racket[#:omit-root-binding] is specified, the original
 @racket[id] is bound to @racket[pattern-id] when used in match patterns and to
 @racket[constructor-id] when used as an expression. Use @racket[
 #:omit-root-binding] when you want control over what @racket[id] is bound to,
 such as when creating a smart constructor.
 
 @(examples
   #:eval (make-evaluator) #:once
   (define-tuple-type point (x y))
   (point 1 2)
   (point? (point 1 2))
   (point-x (point 42 0))
   (point-y (point 42 0))
   (match-define (point (? positive? x) (? negative? y)) (point 3 -3)))}

@defproc[(tuple-type? [v any/c]) boolean?]{
 A predicate for @tech{tuple types}.}

@defproc[
 (tuple-type [name symbol?]
             [size natural?]
             [#:predicate-name predicate-name (or/c symbol? #f) #f]
             [#:constructor-name constructor-name (or/c symbol? #f) #f]
             [#:accessor-name accessor-name (or/c symbol? #f) #f])
 tuple-type?]{
 Constructs a @tech{tuple type} of size @racket[size] and named @racket[name].
 The optional @racket[predicate-name], @racket[constructor-name], and @racket[
 accessor-name] arguments control the result of @racket[object-name] on the
 functions implementing the type. If not provided, @racket[predicate-name]
 defaults to @racket[name]@racketidfont{?}, @racket[constructor-name] defaults
 to @racket[name], and @racket[accessor-name] defaults to @racket[
 name]@racketidfont{-ref}. Two tuple types constructed with the same arguments
 are @racket[equal?]. To make an implementation of a tuple type, see @racket[
 make-tuple-implementation].}

@deftogether[[
 @defproc[(tuple-type-name [type tuple-type?]) symbol?]
 @defproc[(tuple-type-size [type tuple-type?]) natural?]
 @defproc[(tuple-type-predicate-name [type tuple-type?]) symbol?]
 @defproc[(tuple-type-constructor-name [type tuple-type?]) symbol?]
 @defproc[(tuple-type-accessor-name [type tuple-type?]) symbol?]]]{
 Accessors for the various fields of a @tech{tuple type}.}

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

@deftogether[[
 @defproc[(tuple-descriptor-type [descriptor tuple-descriptor?]) tuple-type?]
 @defproc[(tuple-descriptor-predicate [descriptor tuple-descriptor?])
          (-> any/c boolean?)]
 @defproc[(tuple-descriptor-constructor [descriptor tuple-descriptor?])
          procedure?]
 @defproc[(tuple-descriptor-accessor [descriptor tuple-descriptor?])
          (-> (tuple-descriptor-predicate descriptor) natural? any/c)]]]{
 Accessors for the various fields of a tuple @tech{type descriptor}.}

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
 Implements @racket[type] and returns a @tech{tuple type descriptor} for the
 new implementation. The @racket[guard] and @racket[inspector] arguments behave
 the same as the corresponding arguments of @racket[make-struct-type], although
 there are no transparent or prefab tuple types. The @racket[prop-maker]
 argument is similar to the corresponding argument of @racket[
 make-struct-implementation]. By default, tuple types are created with
 properties that make them print and compare in a manner similar to transparent
 structure types --- see @racket[default-tuple-properties] for details.

 @(examples
   #:eval (make-evaluator) #:once
   (define point-descriptor
     (make-tuple-implementation (tuple-type 'point 2)))
   (define point (tuple-descriptor-constructor point-descriptor))
   (define point-x (make-tuple-field-accessor point-descriptor 0 'x))
   (define point-y (make-tuple-field-accessor point-descriptor 1 'y))
   (point 42 888)
   (point-x (point 42 888))
   (point-y (point 42 888)))}

@defproc[
 (make-tuple-field-accessor [descriptor tuple-descriptor?]
                            [pos natural?]
                            [name (or/c symbol? #f)
                             (symbol->string (format "field~a" pos))])
 (-> (tuple-descriptor-predicate descriptor) any/c)]{
 Builds a field accessor function that returns the @racket[pos] field of
 instances of the @tech{tuple type} implemented by @racket[descriptor]. If
 @racket[name] is provided, it is used to derive the name of the function for
 debugging purposes. See @racket[make-tuple-implementation] for usage
 examples.}

@defproc[
 (default-tuple-properties [descriptor uninitialized-tuple-descriptor?])
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
 to behave roughly the same as it does on transparent structure types.

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
 way that @racket[make-constructor-style-printer] prints values.

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
