#lang scribble/manual


@(require (for-label racket/base
                     racket/contract/base
                     rebellion/concurrency/lock)
          (submod rebellion/private/scribble-cross-document-tech doc)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)


@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/concurrency/lock)
    #:private (list 'racket/base)))


@title{Locks}
@defmodule[rebellion/concurrency/lock]


A @deftech{lock}, also called a @deftech{mutex}, is a synchronization primitive that protects access
to a resource shared between @tech/reference{threads}. To protect a value with a lock, require that
threads first @emph{acquire} the lock before interacting with the value and @emph{release} the lock
afterward. Locks are inherently not kill safe, as a thread that dies while holding a lock will never
release it.

Typically, threads cannot acquire locks twice in a row: a thread must release an acquired lock before
attempting to acquire it again. However, @deftech{reentrant} locks can be acquired multiple times by
the same thread. Reentrant locks allow code to acquire a lock before calling other functions that
acquire the same lock.


@defproc[(lock? [v any/c]) boolean?]{
 A predicate for @tech{locks}.}


@defproc[(lock! [lock lock?] [thunk (-> any)]) any]{
 Acquires @racket[lock], calls @racket[thunk], then releases @racket[lock] when control exits
 @racket[thunk] and returns the results of calling @racket[thunk]. A
 @tech/reference{continuation barrier} is installed around the call to @racket[thunk], meaning that
 while control may @emph{leave} @racket[thunk] abnormally (due to an exception, escape continuation,
 or other control operation) it cannot @emph{enter} @racket[thunk] abnormally.

 Strongly prefer this form of locking over calling @racket[lock-acquire!] and
 @racket[lock-release!] manually, as it ensures that the acquired lock is always released.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define counter 0)
    (define counter-lock (make-lock))

    (define (get-and-inc!)
      (lock!
       counter-lock
       (λ ()
         (define previous counter)
         (set! counter (add1 previous))
         previous))))
   
   (get-and-inc!)
   (get-and-inc!)
   counter)}


@defproc[(lock-acquire! [lock lock?]) void?]{
 Acquires @racket[lock], blocking until @racket[lock] is available. Call @racket[lock-release!]
 afterward to ensure that @racket[lock] is made available to other threads. Beware that every call to
 @racket[lock-acquire!] must precede a call to @racket[lock-release!], see
 @secref["deadlock-warning"].}


@defproc[(lock-release! [lock lock?]) void?]{
 Releases @racket[lock], which the current thread must hold. Beware that every call to
 @racket[lock-release!] must follow a call to @racket[lock-acquire!], see
 @secref["deadlock-warning"].}


@defproc[(make-lock [#:reentrant? reentrant? boolean? #true]) lock?]{
 Constructs a new @tech{lock}. If @racket[reentrant?] is true (the default) then the lock is
 @tech{reentrant}.}


@section[#:tag "deadlock-warning"]{A Warning About Deadlocks}


@bold{Acquiring a lock without releasing it is dangerous and can lead to deadlocks.} For
this reason, strongly prefer calling @racket[lock!] instead of @racket[lock-acquire!] and
@racket[lock-release!]. The @racket[lock!] function automatically ensures that acquired locks are
always released. If you must use @racket[lock-acquire!] and @racket[lock-release!], consider using
@racket[dynamic-wind] and a @tech/reference{continuation barrier} to ensure that the lock is acquired
and released correctly.


@(examples
  #:eval (make-evaluator) #:once
  (eval:no-prompt
   (define counter 0)
   (define counter-lock (make-lock))

   (define (get-and-update! updater)
     (lock-acquire! counter-lock)
     (dynamic-wind
      void
      (λ ()
        (call-with-continuation-barrier
         (λ ()
           (define previous counter)
           (set! counter (updater previous))
           previous)))
      (λ () (lock-release! counter-lock)))))
   
  (get-and-update! add1)
  (get-and-update! add1)
  counter)


@section{Read-Write Locks}


A @deftech{read-write lock} is a pair of @tech{locks}, one for read-only access to a resource and one
for write access to a resource. The read lock can be held by multiple threads concurrently so long as
nobody is writing. The write lock can only be held by one thread at a time.


@defproc[(read-write-lock? [v any/c]) boolean?]{
 A predicate for @tech{read-write locks}.}


@defproc[(make-read-write-lock [#:reentrant? reentrant? boolean? #true]) read-write-lock?]{
 Constructs a new @tech{read-write lock}. If @racket[reentrant?] is true (the default) then both the
 read and write locks are @tech{reentrant}.}


@defproc[(read-write-lock-read-lock [rw-lock read-write-lock?]) lock?]{
 Returns the read lock of @racket[rw-lock].}


@defproc[(read-write-lock-write-lock [rw-lock read-write-lock?]) lock?]{
 Returns the write lock of @racket[rw-lock].}
