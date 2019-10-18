#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/set
                     rebellion/collection/set)
          (submod rebellion/private/scribble-cross-document-tech doc))

@title{Sets}
@defmodule[rebellion/collection/set]

@defproc[(immutable-set/c [element-contract chaperone-contract?]) contract?]{
 Constructs a @tech/reference{contract} for immutable @tech/reference{sets}
 containing elements that satisfy @racket[element-contract]. Only @racket[
 equal?]-based sets are allowed. The elements of the set are checked lazily if
 @racket[element-contract] is a chaperone contract, and eagerly if it's a flat
 contract.}

@defproc[(mutable-set/c [element-contract contract?]) contract?]{
 Constructs a @tech/reference{contract} for mutable @tech/reference{sets}
 containing elements that satisfy @racket[element-contract]. Only @racket[
 equal?]-based sets that retain elements strongly are allowed.}

@defthing[empty-set set? #:value (set)]{
 The empty immutable set.}
