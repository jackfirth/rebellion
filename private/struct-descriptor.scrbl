#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/list
                     racket/math
                     rebellion/struct-descriptor)
          (for-syntax racket/base
                      racket/syntax)
          rebellion/private/scribble-evaluator-factory
          scribble/example
          syntax/parse/define)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/struct-descriptor)
    #:private (list 'racket/base)))

@title{Struct Descriptors}
@defmodule[rebellion/struct-descriptor]

@defproc[(struct-descriptor? [v any/c]) boolean?]{
 A predicate for structure type descriptors, initialized or uninitialized.}

@defproc[(initialized-struct-descriptor? [v any/c]) boolean?]{
 A predicate for initialized structure type descriptors, as returned by the
 @racket[make-struct-type/descriptor] function. Implies @racket[
 struct-descriptor?].}

@defproc[(uninitialized-struct-descriptor? [v any/c]) boolean?]{
 A predicate for uninitialized structure type descriptors, as passed to the
 @racket[_prop-maker] argument of @racket[make-struct-type/descriptor]. An
 uninitialized descriptor's constructor, predicate, accessor, and mutator
 functions @bold{must not be called} until the structure type is created ---
 that is, until the corresponding call to @racket[make-struct-type/descriptor]
 returns.}

@defproc[(struct-descriptor-type [descriptor initialized-struct-descriptor?])
         struct-type?]{
 Returns the raw @racket[struct-type?] instance in @racket[descriptor], which
 must be initialized. Uninitialized structure type descriptors do not yet have
 an associated @racket[struct-type?] instance.}

@(define-simple-macro
   (document-struct-descriptor-accessors
    (id:id [field:id contract:expr] ...)
    documentation-content ...)
   #:with [accessor ...]
   (map (λ (id) (format-id id "struct-descriptor-~a" (syntax-e id)
                           #:source id #:props id))
        (syntax->list #'(field ...)))
   (deftogether ((defproc (accessor [id struct-descriptor?]) contract) ...)
     documentation-content ...))

@document-struct-descriptor-accessors[
 (descriptor [super-type (or/c struct-info? #f)]
             [name symbol?]
             [mutable-fields natural?]
             [immutable-fields natural?]
             [auto-fields natural?]
             [constructor procedure?]
             [predicate (-> any/c boolean?)]
             [accessor (-> any/c natural? any/c)]
             [mutator (-> any/c natural? any/c void?)])]{
 Accessors for the various fields of a structure type descriptor.}

@defproc[
 (make-struct-type/descriptor
  [#:name name symbol?]
  [#:mutable-fields mutable-fields natural? 0]
  [#:immutable-fields immutable-fields natural? 0]
  [#:auto-fields auto-fields natural? 0]
  [#:auto-field-value auto-value any/c #f]
  [#:super-type super-type (or/c struct-type? #f) #f]
  [#:property-maker prop-maker
   (-> uninitialized-struct-descriptor?
       (listof (cons/c struct-type-property? any/c)))
   (λ (_) empty)]
  [#:inspector inspector (or/c inspector? 'prefab #f) (current-inspector)]
  [#:guard guard (or/c procedure? #f) #f]
  [#:constructor-name constructor-name (or/c symbol? #f) #f])
 initialized-struct-descriptor?]{
 Like @racket[make-struct-type], but with keyword arguments instead of
 positional arguments and returning a single @racket[struct-descriptor?] value
 instead of multiple values. Additional differences include:

 @itemlist[
 @item{Instead of fields defaulting to mutable and specifying a list of indices
   for immutable fields, this function accepts separate arguments for the number
   of mutable fields and the number of immutable fields. The created struct type
   puts all mutable fields before all immutable fields.}

 @item{Structure type properties are created by the @racket[prop-maker]
   function, which is called with the descriptor before it is initialized. This
   allows property values to refer to the functions associated with the
   descriptor, without requiring users of @racket[make-struct-type/descriptor]
   to create mutually recursive definitions.}

 @item{The @racket[proc-spec] argument is not supported directly. This argument
   is made obsolete by @racket[prop:procedure]; instead of passing @racket[
 proc-spec] callers should include a value for @racket[prop:procedure] in
   the result of @racket[prop-maker].}]

 @(examples
   #:eval (make-evaluator) #:once
   (define point-descriptor
     (make-struct-type/descriptor #:name 'point #:immutable-fields 2))
   (define point (struct-descriptor-constructor point-descriptor))
   (point 1 2))}
