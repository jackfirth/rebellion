#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     rebellion/binary/bit
                     rebellion/binary/byte)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/binary/byte)
    #:private (list 'racket/base)))

@title{Bytes}
@defmodule[rebellion/binary/byte]

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

@defproc[(byte-and [left byte?] [right byte?])
         byte?]{
 Performs a bitwise and operation on @racket[left] using @racket[right] as the mask. Can be used to zero out specific bits of a byte.
  
 @(examples
   #:eval (make-evaluator) #:once
   (code:comment "extract the least-significant bit.")
   (byte-and (byte 1 1 1 1 1 1 1 1) (byte 0 0 0 0 0 0 0 1)) 
   (code:comment "extract bits 7, 5, and 0.")
   (byte-and (byte 1 1 1 1 1 1 1 1) (byte 1 0 1 0 0 0 0 1)))}

@defproc[(byte-or [left byte?] [right byte?])
         byte?]{
 Performs a bitwise or operation on @racket[left] using @racket[right] as the mask. Can be used to set specific bits of a byte without affecting others.
  
 @(examples
   #:eval (make-evaluator) #:once
   (code:comment "set the least-significant bit")
   (byte-or (byte 1 0 0 0 0 0 0 0) (byte 0 0 0 0 0 0 0 1))
   (code:comment "set bits 5, 2, and 1")
   (byte-or (byte 0 0 1 0 1 0 1 0) (byte 0 0 1 0 0 0 1 1)))

@defproc[(byte-not [b byte?])
         byte?]{
 Performs a bitwise not operation on @racket[b], inverting all bits.
  
 @(examples
   #:eval (make-evaluator) #:once
   (byte-not (byte 1 1 1 1 1 1 1 1))
   (byte-not (byte 0 0 0 0 0 0 0 0)))}}

@defproc[(byte-xor [left byte?] [right byte?])
         byte?]{
 Performs a bitwise xor (exclusive or, also known as eor) operation on @racket[left] using @racket[right] as the mask.
  
 @(examples
   #:eval (make-evaluator) #:once
   (byte-xor (byte 0 0 0 1 0 1 0 1) (byte 0 0 1 1 1 1 1 1))
   (code:comment "swap x and y without a temporary variable")
   (let ([x 42] [y 23])
     (set! x (byte-xor x y))
     (set! y (byte-xor y x))
     (set! x (byte-xor x y))
     (list x y)))}

@defproc[(byte-nand [left byte?] [right byte?])
         byte?]{
 Performs a bitwise nand operation on @racket[left] using @racket[right] as the mask.
 Equivalent to (byte-not (byte-and left right)).
  
 @(examples
   #:eval (make-evaluator) #:once
   (byte-nand (byte 1 1 1 1 1 1 1 1) (byte 1 1 1 1 1 1 1 1)) 
   (byte-not (byte-and (byte 1 1 1 1 1 1 1 1) (byte 1 1 1 1 1 1 1 1)))
   (byte-nand (byte 0 0 0 0 0 0 0 0) (byte 0 0 0 0 0 0 0 0))
   (byte-not (byte-and (byte 0 0 0 0 0 0 0 0) (byte 0 0 0 0 0 0 0 0)))
   )}

@defproc[(byte-nor [left byte?] [right byte?])
         byte?]{
 Performs a bitwise nor operation on @racket[left] using @racket[right] as the mask.
 Equivalent to (byte-not (byte-or left right)).
  
 @(examples
   #:eval (make-evaluator) #:once
   (byte-nor (byte 1 1 1 1 1 1 1 1) (byte 1 1 1 1 1 1 1 1)) 
   (byte-not (byte-or (byte 1 1 1 1 1 1 1 1) (byte 1 1 1 1 1 1 1 1)))
   (byte-nor (byte 0 0 0 0 0 0 0 0) (byte 0 0 0 0 0 0 0 0))
   (byte-not (byte-or (byte 0 0 0 0 0 0 0 0) (byte 0 0 0 0 0 0 0 0)))
   )}

@defproc[(byte-xnor [left byte?] [right byte?])
         byte?]{
 Performs a bitwise xnor operation on @racket[left] using @racket[right] as the mask.
 Equivalent to (byte-not (byte-xor left right)).
  
 @(examples
   #:eval (make-evaluator) #:once
   (byte-xnor (byte 1 1 1 1 1 1 1 1) (byte 1 1 1 1 1 1 1 1)) 
   (byte-not (byte-xor (byte 1 1 1 1 1 1 1 1) (byte 1 1 1 1 1 1 1 1)))
   (byte-xnor (byte 0 0 0 0 0 0 0 0) (byte 0 0 0 0 0 0 0 0))
   (byte-not (byte-xor (byte 0 0 0 0 0 0 0 0) (byte 0 0 0 0 0 0 0 0))))}

@defproc[(in-byte [b byte?])
         sequence?]{
 Returns a sequence whose elements are equivalent to the list of bytes that comprise @racket[b].
  
 @(examples
   #:eval (make-evaluator) #:once
   (code:comment "convert byte to a vector of bits")
   (for/vector ([b (in-byte (byte 0 0 1 0 1 0 1 0))])
    b))}

@defproc[(byte-hamming-weight [b byte?])
         (integer-in 0 8)]{
 Returns the Hamming weight of the byte. This is the same as  how many 1's are in the byte.
  
 @(examples
   #:eval (make-evaluator) #:once
   (code:comment "count the number of 1's in the byte")
   (byte-hamming-weight (byte 1 1 1 1 0 0 0 1))
   (byte-hamming-weight (byte 0 0 0 0 0 0 0 0)
   (byte-hamming-weight (byte 1 1 1 1 1 1 1 1))))
}
