#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     rebellion/module/phase)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/module/phase)
    #:private (list 'racket/base)))

@title{Phases}
@defmodule[rebellion/module/phase]

@centered{@emph{"One program's run-time is another program's compile-time."}}

@defproc[(phase? [v any/c]) boolean?]{
 A predicate for @tech{phases}.}

@defproc[(phase [level (or/c exact-integer? #f)]) phase?]{
 Constructs a @tech{phase} at @racket[level], where @racket[#f] is the level of
 the @tech{label phase}.

 @(examples
   #:eval (make-evaluator) #:once
   (phase 0)
   (phase 1)
   (phase -1)
   (phase #f))}

@defproc[(phase-level [ph phase?]) (or/c exact-integer? #f)]{
 Returns the level of @racket[ph], where @racket[#f] is the level of the @tech{
  label phase}.

 @(examples
   #:eval (make-evaluator) #:once
   (phase-level runtime-phase)
   (phase-level compile-phase)
   (phase-level template-phase)
   (phase-level meta-compile-phase)
   (phase-level label-phase))}

@defproc[(phase-shift [ph phase?] [relative-levels exact-integer?]) phase?]{
 Shifts @racket[ph] by @racket[relative-levels], where shifting the @tech{label
  phase} does nothing.

 @(examples
   #:eval (make-evaluator) #:once
   (phase-shift runtime-phase 1)
   (phase-shift meta-compile-phase -2)
   (phase-shift label-phase 5))}

@section{Execution Phases}

@defproc[(execution-phase? [v any/c]) boolean?]{
 A predicate for @deftech{execution phases}, which are @tech{phases} at levels
 where code is actually executed. The only phase where code isn't executed is
 the @tech{label phase}. Implies @racket[phase?] and is mutually exclusive with
 @racket[label-phase?].}

@defproc[(execution-phase [level exact-integer?]) execution-phase?]{
 Equivalent to @racket[(phase level)], but restricted to construct only @tech{
  execution phases}.}

@defproc[(execution-phase-level [ph execution-phase?]) exact-integer?]{
 Equivalent to @racket[(phase-level ph)], but restricted to operate only on
 @tech{execution phases}.}

@defproc[(execution-phase-shift [ph execution-phase?]
                                [relative-levels exact-integer?])
         exact-integer?]{
 Equivalent to @racket[(phase-shift ph relative-levels)], but restricted to
 operate only on @tech{execution phases}.

 @(examples
   #:eval (make-evaluator) #:once
   (execution-phase-shift runtime-phase 2)
   (eval:error (execution-phase-shift label-phase 5)))}

@defthing[runtime-phase phase? #:value (phase 0)]{
 The @deftech{runtime phase}, where expressions are evaluated while the program
 is running.}

@defthing[compile-phase phase? #:value (phase 1)]{
 The @deftech{compile phase}, where expressions are evaluated while
 compiling code in the @tech{runtime phase}.}

@defproc[(compile-phase-for [ph execution-phase?]) execution-phase?]{
 Returns the @tech{phase} of code run when compiling code in @racket[ph],
 effectively shifting @racket[ph] up by one.

 @(examples
   #:eval (make-evaluator) #:once
   (compile-phase-for runtime-phase)
   (compile-phase-for compile-phase)
   (compile-phase-for template-phase))}

@defthing[template-phase phase? #:value (phase -1)]{
 The @deftech{template phase}, where expressions are evaluated in code generated
 by the @tech{runtime phase}.}

@defproc[(template-phase-for [ph execution-phase?]) execution-phase?]{
 Returns the @tech{phase} of code generated by code in @racket[ph], effectively
 shifting @racket[ph] down by one.

 @(examples
   #:eval (make-evaluator) #:once
   (template-phase-for runtime-phase)
   (template-phase-for compile-phase)
   (template-phase-for meta-compile-phase))}

@defthing[meta-compile-phase phase? #:value (phase 2)]{
 The @deftech{meta-compile phase}, where expressions are evaluated while
 compiling code that is itself in the @tech{compile phase}.}

@section{The Label Phase}

@defproc[(label-phase? [v any/c]) boolean?]{
 A predicate for the @tech{label phase}, where no code is executed. Implies
 @racket[phase?] and is mutually exclusive with @racket[execution-phase?].}

@defthing[label-phase phase? #:value (phase #f)]{
  The @deftech{label phase}, where expressions are evaluated in code that
 examines only the @emph{bindings} of other code without actually running it,
 such as documentation.}
