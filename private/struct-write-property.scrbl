#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/struct
                     rebellion/struct-descriptor
                     rebellion/struct-write-property)
          rebellion/private/scribble-evaluator-factory
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/struct-descriptor
                   'rebellion/struct-write-property)
    #:private (list 'racket/base)))

@title{Struct Writer Implementations}
@defmodule[rebellion/struct-write-property]

@defproc[(make-constructor-style-struct-write-property
          [descriptor uninitialized-struct-descriptor?])
         (-> any/c output-port? (or/c #t #f 0 1) void?)]{
 Builds a function suitable for use with @racket[prop:custom-write] using
 @racket[make-constructor-style-printer] and @racket[descriptor]. This function
 is intended to be used with @racket[make-struct-type/descriptor].

 @(examples
   #:eval (make-evaluator) #:once
   (define point-descriptor
     (make-struct-type/descriptor
      #:name 'point
      #:immutable-fields 2
      #:property-maker
      (λ (descriptor)
        (define printer
          (make-constructor-style-struct-write-property descriptor))
        (list (cons prop:custom-write printer)))))
   (define point (struct-descriptor-constructor point-descriptor))
   (point 1 2))}
