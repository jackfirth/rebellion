#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/contract/region
                     racket/match
                     rebellion/base/variant)
          (submod rebellion/private/scribble-cross-document-tech doc)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'racket/match
                   'racket/contract/base
                   'racket/contract/region
                   'rebellion/base/variant)
    #:private (list 'racket/base)))

@title{Variants}
@defmodule[rebellion/base/variant]

A @deftech{variant} is a value tagged with a keyword. Variants are used to
distinguish different kinds of values by name, without knowing anything about
the types of those values.

@defproc[(variant? [v any/c]) variant?]{
 A predicate for @tech{variants}.}

@defproc[(variant [#:<kw> v any/c]) variant?]{
 Constructs a @tech{variant} containing @racket[v] tagged with the given
 keyword, where @racket[#:<kw>] stands for any keyword.

 @(examples
   #:eval (make-evaluator) #:once
   (variant #:success 42)
   (variant #:failure "oops"))

 Additionally, the @racket[variant] constructor may be used as a
 @tech/reference{match expander}.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define (try-add1 v)
      (match v
        [(variant #:success x) (add1 x)]
        [(variant #:failure msg) (error msg)])))

   (try-add1 (variant #:success 42))
   (eval:error (try-add1 (variant #:failure "oops"))))}

@defproc[(variant-value [var variant?]) any/c]{
 Returns the value contained in @racket[var].

 @(examples
   #:eval (make-evaluator) #:once
   (variant-value (variant #:success 42))
   (variant-value (variant #:failure "oops")))}

@defproc[(variant-tag [var variant?]) keyword?]{
 Returns the tag keyword of @racket[var].

 @(examples
   #:eval (make-evaluator) #:once
   (variant-tag (variant #:success 42))
   (variant-tag (variant #:failure "oops")))}

@defproc[(variant-tagged-as? [var variant?] [tag-keyword keyword?]) boolean?]{
 Returns @racket[#t] if @racket[var] is tagged with @racket[tag-keyword],
 returns @racket[#f] otherwise.

 @(examples
   #:eval (make-evaluator) #:once
   (variant-tagged-as? (variant #:success 42) '#:success)
   (variant-tagged-as? (variant #:success 42) '#:failure))}

@defproc[(variant/c [#:<kw> case-contract contract?] ...) contract?]{
 A @tech/reference{contract combinator} for @tech{variants}. Returns a contract
 that enforces that the contracted value is a variant tagged with one of the
 given @racket[#:<kw>]s. If it is, then the corresponding @racket[case-contract]
 is used to check the variant's value. If every @racket[case-contract] is a
 @tech/reference{flat contract} then the returned contract is as well, and
 likewise for @tech/reference{chaperone contracts}.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define/contract (get-success-or-zero var)
      (-> (variant/c #:success number? #:failure string?) number?)
      (match var
        [(variant #:success x) x]
        [(variant #:failure _) 0])))

   (get-success-or-zero (variant #:success 42))
   (eval:error (get-success-or-zero (variant #:success "not a number")))
   (eval:error (get-success-or-zero (variant #:other "whoops"))))}
