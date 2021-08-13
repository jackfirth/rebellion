#lang scribble/manual


@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     racket/sequence
                     racket/set
                     rebellion/base/comparator
                     rebellion/collection/multiset
                     rebellion/streaming/reducer)
          (submod rebellion/private/scribble-evaluator-factory doc)
          (submod rebellion/private/scribble-cross-document-tech doc)
          scribble/example)


@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/base/comparator
                   'rebellion/collection/sorted-set)
    #:private (list 'racket/base)))


@title{Sorted Sets}
@defmodule[rebellion/collection/sorted-set]


A @deftech{sorted set} is a @tech{collection} of distinct elements sorted according to some
@tech{comparator}. Sorted sets may be either mutable or immutable. Two immutable sorted sets are
@racket[equal?] if and only if they contain the same elements and use @racket[equal?] comparators.
