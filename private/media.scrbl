#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     rebellion/base/immutable-string
                     rebellion/base/symbol
                     rebellion/binary/immutable-bytes
                     rebellion/media)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/media)
    #:private (list 'racket/base)))

@title{Media}
@defmodule[rebellion/media]

@;;TODO: make search result content display as "media", not "Media"
@deftech{Media} is binary data produced and consumed by programs according to a
commonly understood format, called a @tech{media type}. Media types aren't
defined by individual programs; rather, the Interned Assigned Numbers Authority
(IANA) maintains a centralized global registry of all known media types. This
process is intended to ensure different programs have the same interpretation of
shared data, but know that even the most rigorously specified media type cannot
guarantee consistency across all implementations.

The @racketmodname[rebellion/media] module does not provide any parsers or
serialization implementations for individual media types. Instead, type-specific
subcollections of @racketmodname[rebellion/media] provide functionality related
to that type. For example, processing of @litchar{text/plain} data would be
implemented in the @racketmodname[rebellion/media/text/plain] module. At
present, @racketmodname[rebellion] does not provide any such implementations. In
the future @racketmodname[rebellion] hopes to provide support for commonly
encountered media types such as @litchar{text/plain}, @litchar{text/csv},
@litchar{application/json}, and @litchar{image/jpeg}.

@;; TODO: example code summarizing the module

@defproc[(media? [v any/c]) boolean?]{
 A predicate for @tech{media}.}

@defproc[(media [type media-type?] [bytes immutable-bytes?]) media?]{
 Constructs a piece of @tech{media} of type @racket[type] containing @racket[
 bytes]. This constructor does not guarantee that @racket[bytes] matches the
 format defined by @racket[type], merely that it is @emph{intended} to match
 @racket[type]. It is up to applications to decide how to handle ill-formed
 media.}

@deftogether[[
 @defproc[(media-get-type [med media?]) media-type?]
 @defproc[(media-bytes [med media?]) immutable-bytes?]]]{
 Accessors for the @tech{media type} and the binary content of @racket[med].}

@section{Media Types}

@(define registry-url "https://www.iana.org/assignments/media-types")

A @deftech{media type} is a named specification registered with the Internet
Assigned Numbers Authority (IANA) that defines a format for binary data, or
@tech{media}. All registered media types and their associated specifications can
be found in the @hyperlink[registry-url]{IANA Media Types Registry}.

A media type consists of a @emph{top-level type} and a @emph{subtype}, and is
registered in one of several @emph{registration trees}. Optionally, a media type
may have a @emph{structured suffix} and/or @emph{parameters}. Media types are
written in the format:

@litchar{top-level "/" [tree "."] subtype ["+" suffix] *[";" parameter]}

For example, @litchar{text/csv; charset=utf-8; header=present} is the
standards-tree media type for UTF-8 encoded CSV files that include an initial
row of column headers. The allowed parameter names and values depend on the type
and are specified in each type's registration.

@defproc[(media-type? [v any/c]) boolean?]{
 A predicate for @tech{media types}.}

@;; TODO: examples
@defproc[(media-type [top-level interned-symbol?] [subtype interned-symbol?]
                     [#:tree tree (or/c #f interned-symbol?) #f]
                     [#:suffix suffix (or/c #f interned-symbol?) #f]
                     [#:parameters params record? empty-record])
         media-type?]{
 Constructs a @tech{media type}.}

@deftogether[[
 @defproc[(media-type-top-level [type media-type?]) interned-symbol?]
 @defproc[(media-type-subtype [type media-type?]) interned-symbol?]
 @defproc[(media-type-tree [type media-type?]) (or/c interned-symbol? #f)]
 @defproc[(media-type-suffix [type media-type?]) (or/c interned-symbol? #f)]
 @defproc[(media-type-params [type media-type?]) record?]]]{
 Accessors for the various components of a @tech{media type}.}

@;; TODO: examples
@defproc[(media-type->string [type media-type?]) immutable-string?]{
 Formats @racket[type] as a string.}
