#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/struct
                     rebellion/struct-descriptor
                     rebellion/struct-equal-property
                     rebellion/struct-write-property)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/struct-descriptor
                   'rebellion/struct-equal-property)
    #:private (list 'racket/base)))

@title{Struct Equality Implementations}
@defmodule[rebellion/struct-equal-property]

@defproc[(make-struct-equal+hash-property
          [descriptor uninitialized-struct-descriptor?])
         (list/c procedure? procedure? procedure?)]{
 Builds an equality-checking procedure, a hashing procedure, and a secondary
 hashing procedure suitable for use with @racket[prop:equal+hash], each of which
 operate on instances of @racket[descriptor]. All fields in @racket[descriptor]
 are compared and hashed by the returned procedures. This causes @racket[equal?]
 to behave roughly the same as it does on transparent structure types.

 This function is intended to be used with @racket[make-struct-type/descriptor].
 See also @racket[make-constructor-style-struct-write-property], which can
 emulate the printed form of transparent structures.

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
        (define equal+hash (make-struct-equal+hash-property descriptor))
        (list (cons prop:equal+hash equal+hash)))))

   (define point (struct-descriptor-constructor point-descriptor))
   (equal? (point 1 2) (point 1 2)))}
