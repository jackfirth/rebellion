#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     rebellion/type/struct
                     rebellion/type/tuple
                     rebellion/equal+hash
                     rebellion/equal+hash/struct)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/type/struct
                   'rebellion/type/tuple
                   'rebellion/equal+hash
                   'rebellion/equal+hash/struct)
    #:private (list 'racket/base)))

@title{Equality and Hashing Implementations}
@defmodule[rebellion/equal+hash]

@defproc[(make-accessor-based-equal+hash [accessor (-> any/c natural? any/c)]
                                         [size natural?])
         equal+hash/c]{
 Builds an equality-checking function, a hashing function, and a secondary
 hashing function suitable for use with @racket[prop:equal+hash]. These
 functions extract @racket[size] fields from values using @racket[accessor] and
 recursively compare and hash them. This function is typically not used
 directly; instead clients are expected to use one of @racket[
 make-struct-equal+hash] or @racket[make-tuple-equal+hash].}

@defthing[equal+hash/c contract?
          #:value (list/c procedure? procedure? procedure?)]

@section{Struct Equality and Hashing}
@defmodule[rebellion/equal+hash/struct]

@defproc[(make-struct-equal+hash [descriptor struct-descriptor?])
         equal+hash/c]{
 Builds an equality-checking function, a hashing function, and a secondary
 hashing function suitable for use with @racket[prop:equal+hash], each of which
 operate on instances of @racket[descriptor]. All fields in @racket[descriptor]
 are compared and hashed by the returned procedures. This causes @racket[equal?]
 to behave roughly the same as it does on transparent structure types.

 @(examples
   #:eval (make-evaluator) #:once
   (struct opaque-point (x y))
   (equal? (opaque-point 1 2) (opaque-point 1 2))

   (define point-descriptor
     (make-struct-type/descriptor
      #:name 'point
      #:immutable-fields 2
      #:property-maker
      (λ (descriptor)
        (define equal+hash (make-struct-equal+hash descriptor))
        (list (cons prop:equal+hash equal+hash)))))

   (define point (struct-descriptor-constructor point-descriptor))
   (equal? (point 1 2) (point 1 2)))}
