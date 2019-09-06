#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     rebellion/base/option
                     rebellion/syntax/annotation))

@title{Syntax Annotations}
@defmodule[rebellion/syntax/annotation]

A @deftech{syntax annotation} is a compile-time value that is attached to a
binding. Each annotation has two parts: an @deftech{annotation type} indicating
what kind of annotation this is, and a @tech{transformer binding} (i.e. a
binding created with @racket[define-syntax]) containing the annotation's value.
The annotation's type specifies a @tech{contract} on the value retrieved by
calling @racket[syntax-local-value] on its transformer binding. Any binding ---
including macro bindings --- can have annotations, but a binding cannot have
more than one annotation of the same type.

Annotations are attached to bindings using an @deftech{annotation transformer}.
An annotation transformer stashes a set of annotations and the original,
unannotated identifier. When used normally, an annotation transformer acts as a
@tech{rename transformer} of the original identifier. But when used with
@racket[syntax-local-annotation-value], the annotation transformer looks up an
annotation of a given type and returns its compile-time value with @racket[
 syntax-local-value].

@defproc[(annotation-transformer? [v any/c]) boolean?]{
 A predicate for @tech{annotation transformers}. Implies @racket[
 rename-transformer?].}

@defproc[(annotation-type? [v any/c]) boolean?]{
 A predicate for @tech{annotation types}.}

@defproc[(make-annotation-type [name (or/c interned-symbol? #f)])
         annotation-type?]{
 Creates a new @tech{annotation type} named @racket[name].}

@defproc[(annotation-type/c [domain contract?]) contract?]{
 A @tech{contract combinator} that creates a contract for @tech{annotation
  types}. The contract restricts the use of the annotation type to storing
 compile-time values matching @racket[domain].}

@defproc[(annotation [type annotation-type?] [id identifier?]) annotation?]{
 Constructs an @tech{annotation} of type @racket[type] with compile-time
 information stored in @racket[id]. If @racket[type] has a contract attached
 with @racket[annotation-type/c], then the result of applying @racket[
 syntax-local-value] to @racket[id] must satisfy that contract.}

@defproc[(syntax-local-annotation-value [id identifier?]
                                        [ann-type annotation-type?])
         option?]

@defproc[(make-annotation-transformer [original-id identifier?]
                                      [annotations annotation-dict?])
         annotation-transformer?]

@defproc[(annotation-dict? [v any/c]) boolean?]
