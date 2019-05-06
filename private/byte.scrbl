#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     rebellion/bit
                     rebellion/byte)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/byte)
    #:private (list 'racket/base)))

@title{Bytes}
@defmodule[rebellion/byte]

A @deftech{byte} is a sequence of eight @tech{bits}, represented as an exact
integer between @racket[0] and @racket[255].

@defproc[(byte [a bit?] [b bit?] [c bit?] [d bit?]
               [e bit?] [f bit?] [g bit?] [h bit?])
         byte?]{
 Constructs a byte from the eight given bits.

 @(examples
   #:eval (make-evaluator) #:once
   (byte 0 0 0 0 0 0 0 0)
   (byte 1 1 1 1 1 1 1 1)
   (byte 0 0 1 0 0 0 0 0)
   (byte 1 0 0 0 0 0 0 1))}

@defproc[(byte-ref [b byte?] [pos (integer-in 0 7)]) bit?]{
 Returns the @tech{bit} at index @racket[pos] in @racket[b]. Indices are
 numbered from left to right and start at zero.
                                                           
 @(examples
   #:eval (make-evaluator) #:once
   (byte-ref (byte 0 0 1 1 0 1 0 1) 0)
   (byte-ref (byte 0 0 1 1 0 1 0 1) 2)
   (byte-ref (byte 0 0 1 1 0 1 0 1) 7))}

@defproc[(byte-clear-leftmost-bits [b byte?] [num-bits (integer-in 0 8)])
         byte?]{
 Sets the left @racket[num-bits] bits of @racket[b] to zero.
                
 @(examples
   #:eval (make-evaluator) #:once
   (byte-clear-leftmost-bits (byte 1 1 1 0 1 0 1 0) 2)
   (byte-clear-leftmost-bits (byte 1 1 1 0 1 0 1 0) 5)
   (byte-clear-leftmost-bits (byte 1 1 1 0 1 0 1 0) 8))}

@defproc[(byte-clear-rightmost-bits [b byte?] [num-bits (integer-in 0 8)])
         byte?]{
 Sets the right @racket[num-bits] bits of @racket[b] to zero.
                
 @(examples
   #:eval (make-evaluator) #:once
   (byte-clear-rightmost-bits (byte 1 1 1 0 1 0 1 0) 2)
   (byte-clear-rightmost-bits (byte 1 1 1 0 1 0 1 0) 4)
   (byte-clear-rightmost-bits (byte 1 1 1 0 1 0 1 0) 7))}

@defproc[(byte-drop-rightmost-bits [b byte?] [num-bits (integer-in 0 8)])
         byte?]{
 Deletes the rightmost @racket[num-bits] of @racket[b], shifting all remaining
 bits to the right and inserting zeros to the left.
                
 @(examples
   #:eval (make-evaluator) #:once
   (byte-drop-rightmost-bits (byte 1 1 1 0 1 0 1 0) 2)
   (byte-drop-rightmost-bits (byte 1 1 1 0 1 0 1 0) 4)
   (byte-drop-rightmost-bits (byte 1 1 1 0 1 0 1 0) 7))}
