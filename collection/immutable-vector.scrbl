#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     rebellion/collection/immutable-vector))

@title{Immutable Vectors}
@defmodule[rebellion/collection/immutable-vector]

An @deftech{immutable vector} is an ordinary @racket[vector?] that is immutable
in the sense of @racket[immutable?]. The @racketmodname[
 rebellion/collection/immutable-vector] module provides functions and predicates that
always accept and return immutable vectors. These functions should always be
used when a vector is known to be immutable, instead of using functions that
work on either mutable or immutable vectors. This is analogous to how mutable
lists are a completely separate data type from ordinary immutable lists.

@defproc[(immutable-vector? [v any/c]) boolean?]{
 A predicate for @tech{immutable vectors}. Implies @racket[vector?] and @racket[
 immutable?].}

@defproc[(make-immutable-vector [size natural?] [v any/c]) immutable-vector?]
@defproc[(immutable-vector [v any/c] ...) immutable-vector?]
@defproc[(immutable-vector-length [vec immutable-vector?]) natural?]
@defproc[(immutable-vector-ref [vec immutable-vector?] [pos natural?]) any/c]
@defproc[(immutable-vector->list [vec immutable-vector?]) list?]
@defproc[(list->immutable-vector [lst list?]) immutable-vector?]

@defproc[(immutable-vector->values [vec immutable-vector?]
                                   [start natural? 0]
                                   [end natural? (immutable-vector-length vec)])
         any]

@defproc[(build-immutable-vector [size natural?] [f (-> natural? any/c)])
         immutable-vector?]

@defproc[(immutable-vector-map [f procedure?] [vec immutable-vector?] ...+)
         immutable-vector?]

@defproc[(immutable-vector-append [vec immutable-vector?] ...)
         immutable-vector?]

@defproc[(immutable-vector-take [vec immutable-vector?] [n natural?])
         immutable-vector?]

@defproc[(immutable-vector-take-right [vec immutable-vector?] [n natural?])
         immutable-vector?]

@defproc[(immutable-vector-drop [vec immutable-vector?] [n natural?])
         immutable-vector?]

@defproc[(immutable-vector-drop-right [vec immutable-vector?] [n natural?])
         immutable-vector?]

@defproc[(immutable-vector-split-at [vec immutable-vector?] [pos natural?])
         (values immutable-vector? immutable-vector?)]

@defproc[(immutable-vector-split-at-right [vec immutable-vector?]
                                          [pos natural?])
         (values immutable-vector? immutable-vector?)]

@defproc[(immutable-vector-copy [vec immutable-vector?]
                                [start natural?]
                                [end natural?])
         immutable-vector?]

@defproc[(immutable-vector-filter [pred predicate/c] [vec immutable-vector?])
         immutable-vector?]

@defproc[(immutable-vector-filter-not [pred predicate/c]
                                      [vec immutable-vector?])
         immutable-vector?]

@defproc[(immutable-vector-count [f procedure?] [vec immutable-vector?] ...+)
         natural?]

@defproc[(immutable-vector-argmin [f (-> any/c real?)] [vec immutable-vector?])
         any/c]

@defproc[(immutable-vector-argmax [f (-> any/c real?)] [vec immutable-vector?])
         any/c]

@defproc[(immutable-vector-member [v any/c] [vec immutable-vector?])
         (or/c natural? #f)]

@defproc[(immutable-vector-memv [v any/c] [vec immutable-vector?])
         (or/c natural? #f)]

@defproc[(immutable-vector-memq [v any/c] [vec immutable-vector?])
         (or/c natural? #f)]

@defproc[(immutable-vector-sort [vec immutable-vector?]
                                [less-than? (-> any/c any/c boolean?)]
                                [start natural? 0]
                                [end natural? (immutable-vector-length vec)]
                                [#:key key (-> any/c any/c) values]
                                [#:cache-keys? cache-keys? boolean? #f])
         immutable-vector?]

@defthing[empty-immutable-vector empty-immutable-vector?]
@defproc[(empty-immutable-vector? [v any/c]) boolean?]
@defproc[(nonempty-immutable-vector? [v any/c]) boolean?]
