#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     rebellion/binary/immutable-bytes)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/binary/immutable-bytes)
    #:private (list 'racket/base)))

@title{Immutable Bytestrings}
@defmodule[rebellion/binary/immutable-bytes]

An @deftech{immutable bytestring} is a @racket[bytes?] value that is immutable,
in the sense of @racket[immutable?]. The @racketmodname[
 rebellion/binary/immutable-bytes] module provides functions and predicates that
always accept and return immutable bytestrings.

@defproc[(immutable-bytes? [v any/c]) boolean?]{
 A predicate for @tech{immutable bytestrings}. Implies @racket[bytes?] and
 @racket[immutable?].}
