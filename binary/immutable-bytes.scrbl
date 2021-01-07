#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     rebellion/binary/immutable-bytes
                     rebellion/base/immutable-string))

@title{Immutable Bytestrings}
@defmodule[rebellion/binary/immutable-bytes]

An @deftech{immutable bytestring} is a @racket[bytes?] value that is immutable,
in the sense of @racket[immutable?]. The @racketmodname[
 rebellion/binary/immutable-bytes] module provides functions and predicates that
always accept and return immutable bytestrings.

@defproc[(immutable-bytes? [v any/c]) boolean?]{
 A predicate for @tech{immutable bytestrings}. Implies @racket[bytes?] and
 @racket[immutable?].}

@defproc[(make-immutable-bytes [size natural?] [b byte?]) immutable-bytes?]
@defproc[(immutable-bytes [b byte?] ...) immutable-bytes?]
@defproc[(immutable-bytes-length [bstr immutable-bytes?]) natural?]
@defproc[(immutable-bytes-ref [bstr immutable-bytes?] [pos natural?]) byte?]

@defproc[(immutable-subbytes [bstr immutable-bytes?]
                             [start natural?]
                             [end natural? (immutable-bytes-length bstr)])
         immutable-bytes?]

@defproc[(immutable-bytes-append [bstr immutable-bytes?] ...) immutable-bytes?]
@defproc[(immutable-bytes->list [bstr immutable-bytes?]) (listof byte?)]
@defproc[(list->immutable-bytes [lst (listof byte?)]) immutable-bytes?]

@defproc[(immutable-bytes=? [bstr1 immutable-bytes?]
                            [bstr2 immutable-bytes?] ...)
         boolean?]

@defproc[(immutable-bytes<? [bstr1 immutable-bytes?]
                            [bstr2 immutable-bytes?] ...)
         boolean?]

@defproc[(immutable-bytes>? [bstr1 immutable-bytes?]
                            [bstr2 immutable-bytes?] ...)
         boolean?]

@defproc[(immutable-bytes->string/utf-8
          [bstr immutable-bytes?]
          [err-char (or/c char? #f)]
          [start natural? 0]
          [end natural? (immutable-bytes-length bstr)])
         immutable-string?]

@defproc[(immutable-bytes->string/locale
          [bstr immutable-bytes?]
          [err-char (or/c char? #f)]
          [start natural? 0]
          [end natural? (immutable-bytes-length bstr)])
         immutable-string?]

@defproc[(immutable-bytes->string/latin-1
          [bstr immutable-bytes?]
          [err-char (or/c char? #f)]
          [start natural? 0]
          [end natural? (immutable-bytes-length bstr)])
         immutable-string?]

@defproc[(immutable-string->bytes/utf-8
          [str immutable-string?]
          [err-byte (or/c byte? #f)]
          [start natural? 0]
          [end (immutable-string-length str)])
         immutable-bytes?]

@defproc[(immutable-string->bytes/locale
          [str immutable-string?]
          [err-byte (or/c byte? #f)]
          [start natural? 0]
          [end (immutable-string-length str)])
         immutable-bytes?]

@defproc[(immutable-string->bytes/latin-1
          [str immutable-string?]
          [err-byte (or/c byte? #f)]
          [start natural? 0]
          [end (immutable-string-length str)])
         immutable-bytes?]

@defproc[(immutable-string-utf-8-length
          [str immutable-string?]
          [start natural? 0]
          [end natural? (immutable-string-length str)])
         natural?]

@defproc[(immutable-bytes-utf-8-length
          [bstr immutable-bytes?]
          [err-char (or/c char? #f)]
          [start natural? 0]
          [end natural? (immutable-bytes-length bstr)])
         (or/c natural? #f)]

@defproc[(immutable-bytes-utf-8-ref
          [bstr immutable-bytes?]
          [pos natural?]
          [err-char (or/c char? #f)]
          [start natural? 0]
          [end natural? (immutable-bytes-length bstr)])
         (or/c char? #f)]

@defproc[(immutable-bytes-utf-8-index
          [bstr immutable-bytes?]
          [pos natural?]
          [err-char (or/c char? #f)]
          [start natural? 0]
          [end natural? (immutable-bytes-length bstr)])
         (or/c natural? #f)]

@defproc[(immutable-bytes-convert
          [converter bytes-converter?]
          [source immutable-bytes?]
          [start natural? 0]
          [end natural? (immutable-bytes-length source)])
         (values immutable-bytes?
                 natural?
                 (or/c 'complete 'continues 'aborts 'error))]

@defproc[(immutable-bytes-convert-end [converter bytes-converter?])
         immutable-bytes?]

@defproc[(immutable-bytes-append* [bstr immutable-bytes?] ...
                                  [bstrs (listof immutable-bytes?)])
         immutable-bytes?]

@defproc[(immutable-bytes-join [bstrs (listof immutable-bytes?)]
                               [sep immutable-bytes?])
         immutable-bytes?]
