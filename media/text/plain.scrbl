#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     rebellion/binary/immutable-bytes
                     rebellion/media/text/plain)
          (submod rebellion/private/scribble-evaluator-factory doc)
          (submod rebellion/private/scribble-cross-document-tech doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/media/text/plain)
    #:private (list 'racket/base)))

@title{Text Media}
@defmodule[rebellion/media/text/plain]

@(define charset-registry-url
   "https://www.iana.org/assignments/character-sets/character-sets.xhtml")

@deftech{Text} is a type of @tech{media} that represents a sequence of
@tech/guide{characters} encoded in a bytestring according to a standardized set of
rules called a @deftech{character set}, or @deftech{charset} for short. Charsets
must be registered with the IANA and their names are assigned in the @hyperlink[
 charset-registry-url]{IANA Character Set Registry}.

Text values are very similar to @tech/guide{strings}. The primary difference between
the two is in how character encoding and decoding are handled:

@itemlist[
 @item{Strings are all encoded with the same charset, which is a Unicode
  encoding internal to Racket. Conversion between strings and bytes requires
  explicitly reencoding the string.}

 @item{Text values specify their charset directly, and different text values can
  have different charsets. Constructing a text value from bytes doesn't require
  performing any decoding --- that's only needed when changing the text's
  charset or when converting the text into a string.}]

This difference is important when a program produces and consumes textual data
without examining it, such as one that copies files from one computer to
another. In such cases, the text can be handled without @emph{ever} changing its
encoding, completely avoiding the cost of converting its bytes back and forth
between string representations.

@defproc[(text? [v any/c]) boolean?]{
 A predicate for @tech{text}.}

@defproc[(text [charset charset?] [bytes immutable-bytes?]) text?]{
 Constructs @tech{text} from the given @racket[bytes] and @racket[charset]. This
 function performs no validation whatsoever that @racket[bytes] conforms to the
 rules of @racket[charset]; a noncomforming @racket[bytes] instead results in an
 error when the constructed text is converted to a string or reencoded with a
 different charset. This function runs in constant time and constant space.}

@defproc[(text-charset [txt text?]) charset?]{
 Returns the @tech{charset} of @racket[txt].}

@defproc[(text-bytes [txt text?]) charset?]{
 Returns the bytes of @racket[txt].}

@defproc[(charset? [v any/c]) boolean?]{
 A predicate for @tech{charsets}.}

@defthing[utf-8 charset?]{
 The Unicode UTF-8 @tech{character set}.}

@defthing[us-ascii]{
 The ASCII @tech{character set}.}

@defproc[(text-media? [m media?]) boolean?]{
 A predicate for @tech{media} whose top level type is @litchar{text}, such as
 @racket[text/plain] and @racket[text/csv].}

@defproc[(text/plain [#:charset charset charset?]) media-type?]{
 Constructs the @tech{media type} for @tech{text} encoded according to @racket[
 charset].}

@defproc[(text->media [txt text?]) text-media?]{
 Converts @racket[txt] into @tech{media} of type @racket[text/plain].}

@defproc[(media->text [m text-media?]) text?]{
 Converts @racket[m] into @tech{text}.}
