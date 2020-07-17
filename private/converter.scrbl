#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     rebellion/base/converter
                     rebellion/base/symbol)
          (submod rebellion/private/scribble-cross-document-tech doc)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/base/converter)
    #:private (list 'racket/base)))

@title{Converters}
@defmodule[rebellion/base/converter]

A @deftech{converter} is an object that can convert back and forth between two
kinds of values: the converter's @deftech{conversion domain} and
@deftech{conversion range}. Converters need not be exact one-to-one mappings,
more commonly a converter is @emph{lossy} and may lose some information by
normalizing converted values. However, repeatedly converting a value back and
forth is not expected to lose more information than converting it back and forth
a single time. Converters are typically named in the pattern
@racketidfont{domain<->range}.

@(examples
  #:eval (make-evaluator) #:once
  (convert-forward number<->string 42)
  (convert-backward number<->string "42")
  (convert-backward number<->string "7.500"))

@defproc[(converter? [v any/c]) boolean?]{
 A predicate for @tech{converters}.}

@defproc[(convert-forward [converter converter?] [input any/c]) any/c]{
 Converts @racket[input] from the domain of @racket[converter] to its range.

 @(examples
   #:eval (make-evaluator) #:once
   (convert-forward number<->string 42)
   (convert-forward string<->symbol "apple"))}

@defproc[(convert-backward [converter converter?] [input any/c]) any/c]{
 Converts @racket[input] from the range of @racket[converter] to its domain.

 @(examples
   #:eval (make-evaluator) #:once
   (convert-backward number<->string "100")
   (convert-backward string<->symbol 'banana))}

@defproc[(make-converter
          [forward-function (-> any/c any/c)]
          [backward-function (-> any/c any/c)]
          [#:name name (or/c interned-symbol? #f) #f])
         converter?]{
 Constructs a @tech{converter} named @racket[name] that uses
 @racket[forward-function] and @racket[backward-function] to convert values.
 The two functions are expected to be inverses of each other, however they are
 allowed to lose some information by normalizing their outputs. More formally,
 @racket[(forward-function _x)] should have the same result as
 @racket[(forward-function (backward-function (forward-function _x)))] and
 @racket[(backward-function _y)] should have the same result as
 @racket[(backward-function (forward-function (backward-function _y)))].

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define lowercase<->uppercase
      (make-converter string-upcase string-downcase)))
   (convert-forward lowercase<->uppercase "foo")
   (convert-backward lowercase<->uppercase "FOO")
   (convert-forward lowercase<->uppercase "Foo"))}

@defproc[(converter/c [domain-contract contract?] [range-contract contract?])
         contract?]{
 A @tech/reference{contract combinator} for @tech{converters}. Constructs a
 contract that recognizes converters which convert from values satisfying
 @racket[domain-contract] to values satisfying @racket[range-contract].}

@section{Predefined Converters}

@defthing[number<->string (converter/c number? string?)]{
 A @tech{converter} between numbers and strings. Numbers are turned into strings
 using @racket[number->string], and strings are parsed into numbers using
 @racket[string->number]. The converter accepts both mutable and immutable
 strings as input, but it only outputs immutable strings.

 @(examples
   #:eval (make-evaluator) #:once
   (convert-forward number<->string 42)
   (convert-backward number<->string "42"))}

@defthing[string<->symbol (converter/c string? symbol?)]{
 A @tech{converter} between strings and symbols. The converter accepts mutable
 strings, immutable strings, interned symbols, unreadable symbols, and
 uninterned symbols as inputs, but it only outputs immutable strings and
 interned symbols.

 @(examples
   #:eval (make-evaluator) #:once
   (convert-forward string<->symbol "grape")
   (convert-backward string<->symbol 'pineapple)
   (convert-backward string<->symbol (gensym)))}

@defthing[string<->keyword (converter/c string? keyword?)]{
 A @tech{converter} between strings and keywords. The converter accepts both
 mutable and immutable strings as inputs, but it only outputs immutable strings.

 @(examples
   #:eval (make-evaluator) #:once
   (convert-forward string<->keyword "jazz")
   (convert-backward string<->keyword '#:blues))}

@defthing[symbol<->keyword (converter/c symbol? keyword?)]{
 A @tech{converter} between symbols and keywords. The converter accepts interned
 symbols, unreadable symbols, and uninterned symbols as inputs, but it only
 outputs interned symbols.

 @(examples
   #:eval (make-evaluator) #:once
   (convert-forward symbol<->keyword 'coffee)
   (convert-backward symbol<->keyword '#:tea)
   (convert-forward symbol<->keyword (string->unreadable-symbol "juice"))
   (convert-forward symbol<->keyword (gensym)))}

@defthing[identity-converter converter?]{
 The identity converter, which does nothing at all to its inputs.

 @(examples
   #:eval (make-evaluator) #:once
   (convert-forward identity-converter "coffee")
   (convert-backward identity-converter 42))}

@section{Converter Utilities}

@defproc[(converter-flip [converter converter?]) converter?]{
 Flips @racket[converter], returning a new converter with a swapped notion of
 forward and backward. Converting forward with the flipped converter is
 equivalent to converting backward with @racket[converter] and vice versa.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt (define string<->number (converter-flip number<->string)))
   (convert-forward string<->number "42")
   (convert-backward string<->number 79))}

@defproc[(converter-pipe [converter converter?] ...) converter?]{
 Pipes each @racket[converter] to the next, one after the other, and returns a
 single composite converter. When converting forwards with the returned
 converter, the input is converted with each @racket[converter] in left to right
 order. When converting backwards, it's converted in right to left order. If no
 converters are given, then the returned converter is equivalent to
 @racket[identity-converter].

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define number<->keyword (converter-pipe number<->string string<->keyword)))
   (convert-forward number<->keyword 42)
   (convert-backward number<->keyword '#:375))}
