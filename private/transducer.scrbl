#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     racket/sequence
                     racket/set
                     rebellion/base/comparator
                     rebellion/base/immutable-string
                     rebellion/base/symbol
                     rebellion/base/variant
                     rebellion/collection/list
                     rebellion/streaming/reducer
                     rebellion/streaming/transducer
                     rebellion/streaming/transducer/testing
                     rebellion/type/record)
          (submod rebellion/private/scribble-evaluator-factory doc)
          (submod rebellion/private/scribble-cross-document-tech doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'racket/set
                   'rebellion/base/comparator
                   'rebellion/base/immutable-string
                   'rebellion/collection/list
                   'rebellion/streaming/reducer
                   'rebellion/streaming/transducer
                   'rebellion/streaming/transducer/testing
                   'rebellion/type/record)
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

@section{Standard Transducers}

@defproc[(mapping [f (-> any/c any/c)]) transducer?]{
 Constructs a @tech{transducer} that applies @racket[f] to input elements and
 emits the returned result downstream.

 @(examples
   #:eval (make-evaluator) #:once
   (transduce (in-range 1 10)
              (mapping (λ (x) (* x x)))
              #:into into-list))}

@defproc[(peeking [handler (-> any/c void?)]) transducer?]{
 Constructs a @tech{transducer} that calls @racket[handler] on each element for
 its side effects. Elements are emitted immediately after @racket[handler] is
 called on them, so if an element is never consumed downstream then @racket[
 handler] won't be called on any later elements.

 This function is intended mostly for debugging, as @racket[(peeking displayln)]
 can be inserted into a transduction pipeline to investigate what elements are
 consumed in that part of the pipeline.

 @(examples
   #:eval (make-evaluator) #:once
   (transduce (list 1 2 3 'apple 4 5 6)
              (peeking displayln)
              (taking-while number?)
              #:into into-sum))}

@defproc[(filtering [pred predicate/c]) transducer?]{
 Constructs a @tech{transducer} that passes input elements downstream only when
 they satisfy @racket[pred].

 @(examples
   #:eval (make-evaluator) #:once
   (transduce (in-range 1 10)
              (filtering even?)
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

@defthing[enumerating transducer?]{
 A transducer that emits each element along with its position in the sequence,
 as an @racket[enumerated?] value.

 @(examples
   #:eval (make-evaluator) #:once
   (transduce "cat" enumerating #:into into-list))}

@deftogether[[
 @defproc[(enumerated? [v any/c]) boolean?]
 @defproc[(enumerated [#:element element any/c] [#:position position natural?])
          enumerated?]
 @defproc[(enumerated-element [enum enumerated?]) any/c]
 @defproc[(enumerated-position [enum enumerated?]) natural?]]]{
 Predicate, constructor, and accessors for the enumerated values emitted by the
 @racket[enumerating] transducer.}

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

@defproc[(sorting [comparator comparator? real<=>]
                  [#:key key-function (-> any/c any/c) values])
         transducer?]{
 Constructs a @tech{transducer} that sorts elements in ascending order according
 to @racket[comparator]. The sort is @emph{stable}; the relative order of
 equivalent elements is preserved.

 @(examples
   #:eval (make-evaluator) #:once
   (transduce (list 4 1 2 5 3)
              (sorting)
              #:into into-list)
   (transduce (list "the" "quick" "brown" "fox")
              (sorting string<=>)
              #:into into-list))

 If @racket[key-function] is provided, it is applied to each element and the
 result is tested with @racket[comparator] rather than the element itself.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define-record-type gem (kind weight))
    (define gems
      (list (gem #:kind 'ruby #:weight 17)
            (gem #:kind 'sapphire #:weight 9)
            (gem #:kind 'emerald #:weight 13)
            (gem #:kind 'topaz #:weight 17))))
   
    (transduce gems
               (sorting #:key gem-weight)
               #:into into-list))}

@defproc[(deduplicating [#:key key-function (-> any/c any/c) values])
         transducer?]{
 Constructs a @tech{transducer} that removes duplicate elements from the
 transduced sequence. The relative order of unique elements is preserved.

 @(examples
   #:eval (make-evaluator) #:once
   (transduce "Hello world!" (deduplicating) #:into into-string))

 If @racket[key-function] is provided, is is applied to each element and
 uniqueness is based on the returned key value.

@(examples
  #:eval (make-evaluator) #:once
  (transduce (list "cat" "dog" "CAT" "HORSE" "horse")
             (deduplicating #:key string-foldcase)
             #:into into-list))}

@defproc[(deduplicating-consecutive
          [#:key key-function (-> any/c any/c) values])
         transducer?]{
 Constructs a @tech{transducer} that removes @emph{consecutive} duplicate
 elements from the transduced sequence. The relative order of retained elements
 is preserved. If the input sequence is sorted, or if it merely has all
 duplicates grouped together, then the constructed transducer behaves
 equivalently to @racket[deduplicating] except it consumes a constant amount of
 memory.

 @(examples
   #:eval (make-evaluator) #:once
   (transduce "Mississippi" (deduplicating-consecutive) #:into into-string))

 If @racket[key-function] is provided, it is applied to each element and
 uniqueness is based on the returned key value.

 @(examples
   #:eval (make-evaluator) #:once
   (transduce (list "cat" "Cat" "CAT" "dog" "HORSE" "horse" "Dog")
              (deduplicating-consecutive #:key string-foldcase)
              #:into into-list))}

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

@section{The Transduction Protocol}

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

@section{Testing Transducers}
@defmodule[rebellion/streaming/transducer/testing]

This module provides utilities for testing @tech{transducers}.

@defproc[(materializing [subject transducer?]) transducer?]{
 Constructs a @tech{transducer} that passes consumed values to @racket[subject],
 then emits @racket[transducer-event?] structures describing the state
 transitions that @racket[subject] performs, including any consumed or emitted
 values. This is mostly useful when testing transducers, as the emitted events
 can be gathered into a list and asserted against.

 The first emitted event is always @racket[start-event] and the last one is
 always @racket[finish-event]. A @racket[half-close-event] is emitted if
 @racket[subject] was half-closed, which means the upstream sequence ran out of
 elements while @racket[subject] was trying to consume one.

 @(examples
   #:eval (make-evaluator) #:once
   (transduce (in-naturals)
              (materializing (taking 3))
              #:into into-list)
   (eval:no-prompt)
   (transduce (list 1 2)
              (materializing (taking 3))
              #:into into-list))}

@defproc[(transducer-event? [v any/c]) boolean?]{
 A predicate that identifiers transducer events, which are emitted by @racket[
 materializing] to describe the behavior of a @tech{transducer}.}

@deftogether[[
 @defthing[start-event transducer-event?]
 @defthing[half-close-event transducer-event?]
 @defthing[finish-event transducer-event?]]]{
 Constants representing a @tech{transducer} starting, half-closing, or
 finishing, respectively.}

@deftogether[[
 @defthing[consume-event? predicate/c]
 @defproc[(consume-event [v any/c]) consume-event?]
 @defproc[(consume-event-value [event consume-event?]) any/c]]]{
 Predicate, constructor, and accessor for transducer events representing a
 @tech{transducer} consuming a value. The consumed value is wrapped by the
 event. Every @racket[consume-event?] is a @racket[transducer-event?].}

@deftogether[[
 @defthing[emit-event? predicate/c]
 @defproc[(emit-event [v any/c]) emit-event?]
 @defproc[(emit-event-value [event emit-event?]) any/c]]]{
 Predicate, constructor, and accessor for transducer events representing a
 @tech{transducer} emitting a value. The emitted value is wrapped by the event.
 Every @racket[emit-event?] is a @racket[transducer-event?].}

@deftogether[[
 @defthing[half-closed-emit-event? predicate/c]
 @defproc[(half-closed-emit-event [v any/c]) half-closed-emit-event?]
 @defproc[(half-closed-emit-event-value [event half-closed-emit-event?])
          any/c]]]{
 Predicate, constructor, and accessor for transducer events representing a
 half-closed @tech{transducer} emitting a value. The emitted value is wrapped by
 the event. Every @racket[half-closed-emit-event?] is a @racket[
 transducer-event?].}
