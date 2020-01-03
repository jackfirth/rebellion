#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/match
                     racket/math
                     rebellion/collection/keyset
                     rebellion/type/record
                     rebellion/type/struct)
          (submod rebellion/private/scribble-cross-document-tech doc)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/examples)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'racket/match
                   'rebellion/type/record)
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
           default-record-properties])
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

@defproc[(default-record-properties [descriptor record-descriptor?])
         (listof (cons/c struct-type-property? any/c))]

@defproc[(default-record-equal+hash [descriptor record-descriptor?])
         equal+hash/c]

@defproc[(default-record-custom-write [descriptor record-descriptor?])
         custom-write-function/c]
