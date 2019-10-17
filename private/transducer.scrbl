#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     racket/sequence
                     racket/set
                     rebellion/base/immutable-string
                     rebellion/base/symbol
                     rebellion/base/variant
                     rebellion/collection/list
                     rebellion/streaming/reducer
                     rebellion/streaming/transducer)
          (submod rebellion/private/scribble-evaluator-factory doc)
          (submod rebellion/private/scribble-cross-document-tech doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'racket/set
                   'rebellion/base/immutable-string
                   'rebellion/collection/list
                   'rebellion/streaming/reducer
                   'rebellion/streaming/transducer)
    #:private (list 'racket/base)))

@title{Transducers}
@defmodule[rebellion/streaming/transducer]

A @deftech{transducer} is an object that can incrementally transform one
(potentially infinite) sequence of elements into another sequence. Transducers
are state machines; performing a transduction involves @emph{starting} the
transducer to get an initial state, then repeatedly updating that state by
either @emph{consuming} an element from the input sequence or by @emph{emitting}
an element to the output sequence. When the input sequence is exhausted, the
transducer enters a @emph{half closed} state where it may emit more output
elements but it will never consume more input elements. When the transducer
stops emitting elements, its @emph{finisher} is called to clean up any resources
held in the final transduction state. Optionally, a transducer may half close
early, before the input sequence is fully consumed.

@defproc[(transducer? [v any/c]) boolean?]{
 A predicate for @tech{transducers}.}

@defproc[(transduce [seq sequence?]
                    [trans transducer?] ...
                    [#:into red reducer?])
         any/c]{
 Executes a @deftech{transduction pipeline}, alternatively called a
 @deftech{stream pipeline}, which transforms the source @racket[seq] with a
 series of intermediate operations --- represented by the @racket[trans]
 arguments --- then reduces the transformed sequence with @racket[red].

 @(examples
   #:eval (make-evaluator) #:once
   (transduce (in-range 1 20)
              (filtering even?)
              (mapping number->immutable-string)
              #:into (join-into-string ", ")))}

@defproc[(in-transducing [seq sequence?] [trans transducer?]) sequence?]{
 Lazily transduces @racket[seq] with @racket[trans], returning a
 @tech/reference{sequence} that, when iterated, passes the elements of @racket[
 seq] to @racket[trans] as inputs and uses the emitted outputs of @racket[trans]
 as the wrapper sequence's elements.}

@defproc[(filtering [pred predicate/c]) transducer?]{
 Constructs a @tech{transducer} that passes input elements downstream only when
 they satisfy @racket[pred].

 @(examples
   #:eval (make-evaluator) #:once
   (transduce (in-range 1 10)
              (filtering even?)
              #:into into-list))}

@defproc[(mapping [f (-> any/c any/c)]) transducer?]{
 Constructs a @tech{transducer} that applies @racket[f] to input elements and
 emits the returned result downstream.

 @(examples
   #:eval (make-evaluator) #:once
   (transduce (in-range 1 10)
              (mapping (λ (x) (* x x)))
              #:into into-list))}

@defproc[(append-mapping [f (-> any/c sequence?)]) transducer?]{
 Constructs a @tech{transducer} that applies @racket[f] to input elements and
 emits each element in the returned sequence downstream.

 @(examples
   #:eval (make-evaluator) #:once
   (transduce (set 'red 'green 'blue)
              (append-mapping symbol->immutable-string)
              #:into into-string))}

@defproc[(folding [f (-> any/c any/c any/c)] [init any/c]) transducer?]{
 Constructs a @tech{transducer} that folds over the input elements and emits the
 current fold state after each element. Specifically, the transducer starts with
 @racket[init] as its state and, for each input element, applies @racket[f] to
 its current state and the input element returning the next state, which is also
 sent downstream.

 @(examples
   #:eval (make-evaluator) #:once
   (transduce (in-range 1 10)
              (folding + 0)
              #:into into-list))}

@defproc[(taking [amount natural?]) transducer?]{
 Constructs a @tech{transducer} that limits the upstream sequence to its first
 @racket[amount] elements. There is no buffering; each element is consumed and
 emitted downstream before the next one is consumed.

 @(examples
   #:eval (make-evaluator) #:once
   (transduce "hello world"
              (taking 5)
              #:into into-string))}

@defproc[(taking-while [pred predicate/c]) transducer?]{
 Constructs a @tech{transducer} that terminates the upstream sequence as soon as
 @racket[pred] returns false for an element. Each element for which @racket[
 pred] returns true is passed downstream.

 @(examples
   #:eval (make-evaluator) #:once
   (transduce "The quick brown fox"
              (taking-while char-alphabetic?)
              #:into into-string))}

@defproc[(dropping [amount natural?]) transducer?]{
 Constructs a @tech{transducer} that removes the first @racket[amount] elements
 from the transduced sequence, then passes all remaining elements downstream.

 @(examples
   #:eval (make-evaluator) #:once
   (transduce "hello world"
              (dropping 5)
              #:into into-string))}

@defproc[(dropping-while [pred predicate/c]) transducer?]{
 Constructs a @tech{transducer} that removes elements from the transduced
 sequence until @racket[pred] returns false for an element, then passes all
 remaining elements downstream.

 @(examples
   #:eval (make-evaluator) #:once
   (transduce "The quick brown fox"
              (dropping-while char-alphabetic?)
              #:into into-string))}

@defthing[deduplicating transducer?]{
 A @tech{transducer} that removes duplicate elements from the transduced
 sequence. The relative order of unique elements is preserved.

 @(examples
   #:eval (make-evaluator) #:once
   (transduce "Hello world!" deduplicating #:into into-string))}

@defproc[(batching [batch-reducer reducer?]) transducer?]{
 Constructs a @tech{transducer} that collects elements of the transduced
 sequence into batches using @racket[batch-reducer]. Elements are fed into
 @racket[batch-reducer] until it terminates the reduction, then the reduction
 result is emitted downstream. If there are more elements remaining, then the
 @racket[batch-reducer] is restarted to prepare the next batch. When the
 transduced sequence has no more elements, if the last batch is only partially
 complete, then the @racket[batch-reducer]'s finisher is called to produce the
 last batch.

 If @racket[batch-reducer] refuses to consume any elements and immediately
 terminates the reduction every time it's started, then the returned transducer
 raises @racket[exn:fail:contract].

 @(examples
   #:eval (make-evaluator) #:once
   (transduce (in-range 10)
              (batching (reducer-limit into-list 4))
              #:into into-list))}

@defproc[(make-transducer
          [#:starter starter (-> transduction-state/c)]
          [#:consumer consumer (-> any/c transduction-state/c)]
          [#:emitter emitter (-> any/c emission?)]
          [#:half-closer half-closer
           (-> any/c half-closed-transduction-state/c)]
          [#:half-closed-emitter half-closed-emitter
           (-> any/c half-closed-emission?)]
          [#:finisher finisher (-> any/c void?)]
          [#:name name (or/c interned-symbol? #f) #f])
         transducer?]

@defthing[transduction-state/c flat-contract?
          #:value (variant/c #:consume any/c
                             #:emit any/c
                             #:half-closed-emit any/c
                             #:finish any/c)]

@defthing[half-closed-transduction-state/c flat-contract?
          #:value (variant/c #:half-closed-emit any/c
                             #:finish any/c)]

@defproc[(emission? [v any/c]) boolean?]
@defproc[(emission [state transduction-state/c] [value any/c]) emission?]
@defproc[(emission-state [em emission?]) transduction-state/c]
@defproc[(emission-value [em emission?]) any/c]

@defproc[(half-closed-emission? [v any/c]) boolean?]

@defproc[(half-closed-emission [state half-closed-transduction-state/c]
                               [value any/c])
         half-closed-emission?]

@defproc[(half-closed-emission-state [em half-closed-emission?])
         half-closed-transduction-state/c]

@defproc[(half-closed-emission-value [em half-closed-emission?]) any/c]
