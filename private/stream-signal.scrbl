#lang scribble/manual

@title{Stream Signals}
@defmodule[rebellion/streaming/signal]

A @deftech{stream signal} or simply @deftech{signal} is an object that describes
a boolean-valued property over a sequence of values. Once started, a signal is
either @deftech{on} or @deftech{off} and may toggle between these states when
receiving the elements of a sequence. Stream signals may be thought of as
predicates that have state, which may be updated in between sequence elements.

@defproc[(stream-signal? [v any/c]) boolean?]{
 A predicate for @tech{stream signals}.}

@section{Basic Signals}

@defproc[(increasing? [cmp (-> any/c any/c (or/c '< '= '>))]
                      [#:strictly? strictly? boolean? #f])
         stream-signal?]{
 A @tech{stream signal} that is @tech{on} when @racket[cmp] indicates that the
 current sequence element is not smaller than the previous element. If @racket[
 strictly?] is @racket[#t], elements must be greater than their predecessor for
 the signal to be on --- an element that is equal to its predecessor would turn
 the signal off.}

@defproc[(decreasing? [cmp (-> any/c any/c (or/c '< '= '>))]
                      [#:strictly? strictly? boolean? #f])
         stream-signal?]{
 Like @racket[increasing?], but the constructed @tech{signal} is @tech{on} when
 elements are decreasing, not increasing.}

@section{Using Signals in Sequence Transformations}

@defproc[(filtering-on [signal stream-signal?]) transducer?]{
 Constructs a @tech{transducer} that emits elements only when @racket[signal] is
 @tech{on}.}

@defproc[(taking-while-on [signal stream-signal?]) transducer?]{
 Constructs a @tech{transducer} that emits upstream elements as long as @racket[
 signal] is @tech{on}, terminating the sequence as soon as it's @tech{off}.}

@defproc[(dropping-while-on [signal stream-signal?]) transducer?]{
 Constructs a @tech{transducer} that drops upstream elements as long as @racket[
 signal] is @tech{on}. Once it's @tech{off}, upstream elements are emitted
 normally until the sequence ends, even if @racket[signal] turns back
 @tech{on}.}

@section{Constructing Signals}

@defproc[(make-fold-stream-signal [f (-> any/c any/c signal-output?)]
                                  [init any/c]
                                  [#:name name (or/c interned-symbol? #f)])
         signal?]

@defproc[(signal-output? [v any/c]) boolean?]

@defproc[(signal-output [#:state state any/c]
                        [#:on? on? boolean?]) signal-output?]

@defproc[(signal-output-state [output signal-output?]) any/c]

@defproc[(signal-output-on? [output signal-output?]) boolean?]
