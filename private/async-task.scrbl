#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     rebellion/concurrency/async-task
                     rebellion/concurrency/async-task/mutable)
          (submod rebellion/private/scribble-cross-document-tech doc)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/concurrency/async-task)
    #:private (list 'racket/base)))

@title{Asynchronous Tasks}
@defmodule[rebellion/concurrency/async-task]

An @deftech{asynchronous task}, or simply @deftech{async task}, represents the
result of some asynchronous operation. Functions are provided to check the state
of the computation, to wait for its completion, and to extract its result. If
the result of an async task is no longer needed, it can be @tech{cancelled}
to prevent unnecessary work from being done. Async tasks may also have
@deftech{background work} associated with them --- operations that are performed
as part of the task, but which can finish after the task produces a result.

At any point, an async task is in one of four states:

@itemlist[
 @item{The task may be @deftech{running}, meaning that it has not yet produced
  any result, failed, or been cancelled.}
  
 @item{The task may be @deftech{successful}, meaning it produced a result
  without error. Successful tasks may still have running @tech{background work}
  associated with them.}
 
 @item{The task may have @deftech{failed}, meaning it raised an error instead of
  producing a result. Like successful tasks, failed tasks may still have running
  @tech{background work}.}
 
 @item{The task may have been @deftech{cancelled}, indicating that something
  requested the task be shut down because its result is no longer needed. Unlike
  successful and failed tasks, when a task is cancelled its @tech{background
   work} is cancelled as well. This behavior can be circumvented when creating
  tasks by wrapping the background work to be @tech{non-cancellation
   propagating}.}]

An async task that is successful, failed, or cancelled is called a @deftech{
 ready} task. Additionally, an async task may be @deftech{finished} --- this
only occurs once the task is @tech{ready} and all @tech{background work} is
finished. Cancelled tasks are always finished, because cancelling a task also
cancels its background work.

@defproc[(async-task? [v any/c]) boolean?]{
 A predicate for @tech{async tasks}.}

@section{Async Task Subtypes}

@defproc[(running-async-task? [v any/c]) boolean?]{
 A predicate for @tech{async tasks} that are still @tech{running}. Implies
 @racket[async-task?].}

@defproc[(ready-async-task? [v any/c]) boolean?]{
 A predicate for @tech{async tasks} that are @tech{ready}. Implies @racket[
 async-task?].}

@defproc[(successful-async-task? [v any/c]) boolean?]{
 A predicate for @tech{async tasks} that are @tech{successful}. Implies @racket[
 ready-async-task?].}

@defproc[(failed-async-task? [v any/c]) boolean?]{
 A predicate for @tech{async tasks} that have @tech{failed}. Implies @racket[
 ready-async-task?].}

@defproc[(finished-async-task? [v any/c]) boolean?]{
 A predicate for @tech{async tasks} that are @tech{finished}. Implies
 @racket[ready-async-task?].}

@defproc[(cancelled-async-task? [v any/c]) boolean?]{
 A predicate for @tech{async tasks} that have benn @tech{cancelled}. Implies
 @racket[finished-async-task?].}

@section{Async Task Constructors}

@defproc[(immediate-async-task [v any/c]) successful-async-task?]{
 Returns an @tech{async task} that is always @tech{successful} and has the
 result @racket[v].}

@defproc[(immediate-failed-async-task [failure exn:fail?]) failed-async-task?]{
 Returns an @tech{async task} that is always @tech{failed} with @racket[
 failure].}

@defproc[(immediate-cancelled-task [reason exn?]) cancelled-async-task?]{
 Returns an @tech{async task} that is always @tech{cancelled} with @racket[
 reason] as the cancellation reason.}

@section{Blocking on Async Tasks}

@defproc[(async-task-get-result! [task async-task?]) any/c]

@defproc[(async-task-await-ready! [task async-task?]) void?]

@defproc[(async-task-await-finished! [task async-task?]) void?]

@section{Synchronizing on Async Tasks}

@defproc[(async-task-get-result-evt [task async-task?]) evt?]{
 Returns a @tech/reference{synchronizable event} that is @tech/reference{ready
  for synchronization} when @racket[task] succeeds, fails, or is cancelled. If
 @racket[task] is successful, the returned event's @tech/reference{
  synchronization result} is the result of @racket[task]. If @racket[task] is
 failed, the synchronization throws an instance of @racket[exn:fail:async]
 containing the task's failure. If @racket[task] is cancelled, synchronization
 throws an instance of @racket[exn:fail:async-cancel] containing the task's
 cancellation reason.}

@defproc[(async-task-await-ready-evt [task async-task?]) (evt/c void?)]{
 Returns a @tech/reference{synchronizable event} that is @tech/reference{ready
  for synchronization} once @racket[task] is @tech{ready}, ignoring any @tech{
  background work}. Once ready, the returned event's @tech/reference{
  synchronization result} is always @racket[void?] regardless of the state of
 @racket[task].}

@defproc[(async-task-finished-evt [task async-task?]) (evt/c void?)]{
 Returns a @tech/reference{synchronizable event} that is @tech/reference{ready
  for synchronization} once @racket[task] is @tech{finished}, including any
 @tech{background work}. From then onwards, the returned event's
 @tech/reference{synchronization result} is always @racket[void?] regardless of
 the state of @racket[task].}

@section{Async Task Cancellation}

@defproc[(async-task-cancel! [task async-task?] [reason exn?]) void?]{
 Cancels @racket[task], with @racket[reason] as the cancellation reason.}

@section{Mutable Async Tasks}
@defmodule[rebellion/concurrency/async-task/mutable]

To construct @tech{async tasks} from callback-based APIs, it is sometimes
necessary to use mutation to communicate the state of a task to its consumers.
For this purpose, a @deftech{mutable async task} may be used. Mutable async
tasks are exactly like regular async tasks and satisfy the @racket[async-task?]
predicate, but they also allow users to explicitly set the state of the task
using functions such as @racket[mutable-async-task-set-result!].

This method of constructing async tasks can be fragile and error-prone --- it's
easy to accidentally create tasks that never finish. Use mutable async tasks
only as a last resort, when the alternatives offered by @racketmodname[
 rebellion/concurrency/async-task] are not suitable for the task at hand.

@defproc[(mutable-async-task? [v any/c]) boolean?]{
 A predicate for @tech{mutable async tasks}. Implies @racket[async-task?].}

@defproc[(make-mutable-async-task) mutable-async-task?]{
 Constructs a new @tech{mutable async task}. The new task will be considered
 @tech{running} until one of @racket[mutable-async-task-set-result!], @racket[
 mutable-async-task-set-failure!], or @racket[async-task-cancel!] is called on
 the task.}

@defproc[(mutable-async-task-set-result!
          [task mutable-async-task?]
          [result any/c]
          [#:unfinished-background-work? unfinished? boolean? #f])
         void?]{
 Sets the result of @racket[task] to @racket[result], provided @racket[task] is
 still @tech{running}. If @racket[task] has already succeeded, failed, or been
 cancelled, a contract failure is raised.

 If @racket[unfinished?] is false (the default), then the state of @racket[task]
 is set to @tech{finished} in addition to being @tech{successful}. If it's true,
 then instead @racket[task] is not marked finished (but is still marked
 successful). In that case, @racket[task] will need to be marked finished at
 some point in the future using @racket[mutable-async-task-set-finished!].}

@defproc[(mutable-async-task-set-failure!
          [task mutable-async-task?]
          [failure exn:fail?]
          [#:unfinished-background-work? unfinished? boolean? #f])
         void?]{
 Fails @racket[task] with @racket[failure] as the failure result, provided
 @racket[task] is still @tech{running}. If @racket[task] has already succeeded,
 failed, or been cancelled, a contract failure is raised.

 If @racket[unfinished?] is false (the default), then the state of @racket[task]
 is set to @tech{finished} in addition to being @tech{failed}. If it's true,
 then instead @racket[task] is not marked finished (but is still marked
 failed). In that case, @racket[task] will need to be marked finished at some
 point in the future using @racket[mutable-async-task-set-finished!].}

@section{Async Tasks v.s. Synchronizable Events}
