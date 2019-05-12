#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     rebellion/media
                     rebellion/media/application/octet-stream)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/media/application/octet-stream)
    #:private (list 'racket/base)))

@title{Octet Streams}
@defmodule[rebellion/media/application/octet-stream]

An @deftech{octet stream} is an immutable byte string whose last byte may
include extra bits of padding, making octet streams semantically similar to
@tech{bitstrings}. Octet streams are @tech{media}, and their @tech{media type}
is @racket[application/octet-stream].

@defproc[(octet-stream? [v any/c]) boolean?]{
 A predicate for @tech{octet streams}.}

@defproc[(application/octet-stream [#:padding padding (integer-in 0 7) 0])
         media-type?]{
 Constructs the @tech{media type} of @tech{octet streams} padded with @racket[
 padding] bits.

 @(examples
   #:eval (make-evaluator) #:once
   (application/octet-stream)
   (application/octet-stream #:padding 6))}

@defproc[(octet-stream [bstr immutable-bytes?]
                       [#:padding padding (integer-in 0 7) 0])
         octet-stream?]{
 Constructs an @tech{octet stream} from @racket[bstr]. If @racket[padding] is
 nonzero, that many bits are ignored from the last byte of @racket[bstr] when
 the octet stream is converted to a @tech{bitstring} with @racket[
 octet-stream->bitstring].

 @(examples
   #:eval (make-evaluator) #:once
   (octet-stream #"Apple")
   (octet-stream #"Apple" #:padding 3))}

@defproc[(octet-stream->bitstring [octets octet-stream?]) bitstring?]{
 Converts @racket[octets] into a @tech{bitstring}.
          
 @(examples
   #:eval (make-evaluator) #:once
   (octet-stream->bitstring (octet-stream #"Apple"))
   (octet-stream->bitstring (octet-stream #"Apple" #:padding 3)))}

@defproc[(octet-stream-bytes [octets octet-stream?]) immutable-bytes?]{
 Returns the bytes contained in @racket[octets].}

@defproc[(octet-stream-padding [octets octet-stream?]) (integer-in 0 7)]{
 Returns the number of padding bits in @racket[octets].}

@defproc[(media->octet-stream [m media?]) octet-stream?]{
 Constructs an @tech{octet stream} containing the bytes of @racket[m]. Octet
 streams are the most general @tech{media type}, so all media can be converted
 to an octet stream regardless of type. However, the returned octet stream will
 only have nonzero padding if @racket[m] has type @racket[
 application/octet-stream] with the @racket[#:padding] parameter set.}

@defproc[(octet-stream->media [octets octet-stream?]) media?]{
 Converts @racket[octets] into @tech{media} of type @racket[
 application/octet-stream] with the padding parameter set if @racket[octets] has
 nonzero padding.

 @(examples
   #:eval (make-evaluator) #:once
   (octet-stream->media (octet-stream #"Apple"))
   (octet-stream->media (octet-stream #"Apple" #:padding 3)))}
