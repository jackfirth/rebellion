#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     rebellion/immutable-string))

@title{Immutable Strings}
@defmodule[rebellion/immutable-string]

An @deftech{immutable string} is an ordinary @racket[string?] that is immutable
in the sense of @racket[immutable?]. The @racketmodname[
 rebellion/immutable-string] module provides functions and predicates that
always accept and return immutable strings.

@defproc[(immutable-string? [v any/c]) boolean?]{
 A predicate for @tech{immutable strings}. Implies @racket[string?] and
 @racket[immutable?].}

@defproc[(make-immutable-string [k natural?] [char char? #\nul])
         immutable-string?]

@defproc[(immutable-string [char char?] ...) immutable-string?]
@defproc[(immutable-string-length [string immutable-string?]) natural?]

@defproc[(immutable-string-ref [string immutable-string?] [k natural?])
         char?]

@defproc[(immutable-substring [string immutable-string?]
                              [start natural?]
                              [end natural? (immutable-string-length string)])
         immutable-string?]

@defproc[(immutable-string-append [string immutable-string?] ...)
         immutable-string?]

@defproc[(immutable-string->list [string immutable-string?]) (listof char?)]
@defproc[(list->immutable-string [lst (listof char?)]) immutable-string?]

@defproc[(build-immutable-string [n natural?] [func (-> natural? char?)])
         immutable-string?]

@defproc[(immutable-string=? [string1 immutable-string?]
                             [string2 immutable-string?] ...)
         boolean?]

@defproc[(immutable-string<? [string1 immutable-string?]
                             [string2 immutable-string?] ...)
         boolean?]

@defproc[(immutable-string<=? [string1 immutable-string?]
                              [string2 immutable-string?] ...)
         boolean?]

@defproc[(immutable-string>? [string1 immutable-string?]
                             [string2 immutable-string?] ...)
         boolean?]

@defproc[(immutable-string>=? [string1 immutable-string?]
                              [string2 immutable-string?] ...)
         boolean?]

@defproc[(immutable-string-ci=? [string1 immutable-string?]
                                [string2 immutable-string?] ...)
         boolean?]

@defproc[(immutable-string-ci<? [string1 immutable-string?]
                                [string2 immutable-string?] ...)
         boolean?]

@defproc[(immutable-string-ci<=? [string1 immutable-string?]
                                 [string2 immutable-string?] ...)
         boolean?]

@defproc[(immutable-string-ci>? [string1 immutable-string?]
                                [string2 immutable-string?] ...)
         boolean?]

@defproc[(immutable-string-ci>=? [string1 immutable-string?]
                                 [string2 immutable-string?] ...)
         boolean?]

@defproc[(immutable-string-upcase [string immutable-string?]) immutable-string?]

@defproc[(immutable-string-downcase [string immutable-string?])
         immutable-string?]

@defproc[(immutable-string-titlecase [string immutable-string?])
         immutable-string?]

@defproc[(immutable-string-foldcase [string immutable-string?])
         immutable-string?]

@defproc[(immutable-string-normalize-nfd [string immutable-string?])
         immutable-string?]

@defproc[(immutable-string-normalize-nfkd [string immutable-string?])
         immutable-string?]

@defproc[(immutable-string-normalize-nfc [string immutable-string?])
         immutable-string?]

@defproc[(immutable-string-normalize-nfkc [string immutable-string?])
         immutable-string?]

@defproc[(immutable-string-locale=? [string1 immutable-string?]
                                    [string2 immutable-string?] ...)
         boolean?]

@defproc[(immutable-string-locale<? [string1 immutable-string?]
                                    [string2 immutable-string?] ...)
         boolean?]

@defproc[(immutable-string-locale>? [string1 immutable-string?]
                                    [string2 immutable-string?] ...)
         boolean?]

@defproc[(immutable-string-ci-locale=? [string1 immutable-string?]
                                       [string2 immutable-string?] ...)
         boolean?]

@defproc[(immutable-string-ci-locale<? [string1 immutable-string?]
                                       [string2 immutable-string?] ...)
         boolean?]

@defproc[(immutable-string-ci-locale>? [string1 immutable-string?]
                                       [string2 immutable-string?] ...)
         boolean?]

@defproc[(immutable-string-locale-upcase [string immutable-string?])
         immutable-string?]

@defproc[(immutable-string-locale-downcase [string immutable-string?])
         immutable-string?]

@defproc[(empty-immutable-string? [v any/c]) boolean?]
@defproc[(nonempty-immutable-string? [v any/c]) boolean?]
@defthing[empty-immutable-string empty-immutable-string?]
@defproc[(symbol->immutable-string [sym symbol?]) immutable-string?]
@defproc[(keyword->immutable-string [kw keyword?]) immutable-string?]

@defproc[(number->immutable-string [z number?] [radix (or/c 2 8 10 16) 10])
         immutable-string?]

@defproc[(immutable-string-join
          [strs (listof immutable-string?)]
          [sep immutable-string? " "]
          [#:before-first before-first immutable-string? ""]
          [#:before-last before-last immutable-string? sep]
          [#:after-last after-last immutable-string? ""])
         immutable-string?]
