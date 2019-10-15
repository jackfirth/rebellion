#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     rebellion/binary/bitstring)
          (submod rebellion/private/scribble-cross-document-tech doc)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/binary/bitstring)
    #:private (list 'racket/base)))

@title{Bitstrings}
@defmodule[rebellion/binary/bitstring]

A @deftech{bitstring} is an immutable, contiguous sequence of @tech{bits}.
Bitstrings are represented compactly; a bitstring of 8N bits consumes N bytes of
memory, plus some constant overhead. Bitstrings implement the @tech/reference{
 sequence} interface.

@defproc[(bitstring? [v any/c]) boolean?]{
 A predicate for @tech{bitstrings}.}

@defproc[(bitstring [b bit?] ...) bitstring?]{
 Constructs a @tech{bitstring} containing the given bits.

 @(examples
   #:eval (make-evaluator) #:once
   (bitstring 1 0 0 1 0)
   (bitstring 0 0 0 0 0 0 0 0
              1 1 1 1 1 1 1 1
              0 1 0 1 0 1 0 1
              0 1 1)
   (bitstring))}

@defthing[empty-bitstring bitstring?]{
 The empty bitstring.}

@defproc[(bitstring-ref [bits bitstring?]
                        [pos (integer-in 0 (bitstring-size bits))])
         bit?]{
 Returns the @tech{bit} at @racket[pos] in @racket[bits].

 @(examples
   #:eval (make-evaluator) #:once
   (define bits (bitstring 1 1 1 0 0 1 0 0 0 1 1))
   (bitstring-ref bits 0)
   (bitstring-ref bits 5)
   (bitstring-ref bits 6)
   (bitstring-ref bits 8))}

@defproc[(bitstring-size [bits bitstring?]) natural?]{
 Returns the number of @tech{bits} in @racket[bits]. Note that bitstrings can
 have sizes that are not multiples of eight.

 @(examples
   #:eval (make-evaluator) #:once
   (bitstring-size (bitstring))
   (bitstring-size (bitstring 1 1 1 1 0 0 0 0))
   (bitstring-size (bitstring 1 1 1 1 0 0 0 0 1)))}

@defproc[(in-bitstring [bits bitstring?]) (sequence/c bit?)]{
 Returns a @tech/reference{sequence} that traverses each bit in @racket[bits].
 Note that bitstrings already implement the sequence interface, but using this
 function in a @racket[for] comprehension may improve performance and error
 messages over using @racket[bits] directly.

 @(examples
   #:eval (make-evaluator) #:once
   (for ([position (in-naturals)]
         [bit (in-bitstring (bitstring 1 1 0 0 1 0 1 0 0 1))]
         #:unless (zero? bit))
     (printf "Bit ~a is set\n" position)))}

@defproc[(bitstring->padded-bytes [bits bitstring?]) immutable-bytes?]{
 Returns an immutable byte string where each byte corresponds to eight bits of
 @racket[bits]. If @racket[bits] has a size that is not a multiple of eight, the
 last byte is constructed by adding zeros to the end of @racket[bits] until the
 size is a multiple of eight. This is a constant-time operation.

 @(examples
   #:eval (make-evaluator) #:once
   (bitstring->padded-bytes (bitstring 0 0 0 0 0 1 1 1))
   (bitstring->padded-bytes (bitstring 0 0 0 0 0 1 1 1 1)))}

@defproc[(bytes->bitstring [bstr immutable-bytes?]
                           [#:padding padding (integer-in 0 7) 0])
         bitstring?]{
 Converts @racket[bstr] into a @tech{bitstring} by converting each of its bytes
 into eight bits. The last @racket[padding] bits are ignored, which allows
 constructing bitstrings with sizes that are not a multiple of eight.

 @(examples
   #:eval (make-evaluator) #:once
   (bytes->bitstring #"apple")
   (bytes->bitstring #"Apple"))}
