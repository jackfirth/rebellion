#lang scribble/manual

@(require (for-label racket/base
                     racket/bool
                     racket/contract/base
                     racket/contract/region
                     racket/math
                     rebellion/base/comparator
                     rebellion/base/immutable-string
                     rebellion/base/symbol
                     rebellion/collection/hash
                     rebellion/collection/list
                     rebellion/streaming/reducer
                     rebellion/streaming/transducer
                     rebellion/type/enum
                     rebellion/type/record
                     rebellion/type/tuple)
          (submod rebellion/private/scribble-cross-document-tech doc)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'racket/contract/base
                   'racket/contract/region
                   'rebellion/base/comparator
                   'rebellion/collection/list
                   'rebellion/streaming/reducer
                   'rebellion/streaming/transducer
                   'rebellion/type/enum
                   'rebellion/type/record
                   'rebellion/type/tuple)
    #:private (list 'racket/base)))

@(define lexicographic-order-url "https://en.wikipedia.org/wiki/Lexicographic_order")


@title{Comparators}
@defmodule[rebellion/base/comparator]

A @deftech{comparator} is an object that compares two values and determines
whether one is greater than the other, or whether they are equivalent. This
comparison must respect some @deftech{total ordering}, meaning that for any two
values @racket[_x] and @racket[_y]:

@itemlist[
 @item{If @racket[_x] is less than @racket[_y], then @racket[_y] must be greater
  than @racket[x]. The reverse must hold true if @racket[_x] is greater than
  @racket[_y].}

 @item{If @racket[_x] is equivalent to @racket[_y], then @racket[_y] must be
  equivalent to @racket[_x].}

 @item{If @racket[_x] is @racket[equal?] to @racket[_y], they must be
  equivalent.}]

Note that the third requirement above does @bold{not} imply that all equivalent
values must be @racket[equal?]. For example, the @racket[real<=>] comparator
considers @racket[3] and @racket[3.0] equivalent, but @racket[(equal? 3 3.0)]
returns false. A comparator for which all equivalent values are also equal is
said to be @deftech{consistent with equality}, and comparators which do not
satisfy this stronger property are @deftech{inconsistent with equality}. All
comparators defined in @racketmodname[rebellion/base/comparator] are consistent
with equality unless otherwise stated.


@defproc[(comparator? [v any/c]) boolean?]{
 A predicate for @tech{comparators}.}


@defproc[(compare [comparator comparator?] [left any/c] [right any/c])
         comparison?]{
 Returns whether @racket[left] is less than, greater than, or equivalent to
 @racket[right].

 @(examples
   #:eval (make-evaluator) #:once
   (compare real<=> 5 8)
   (compare string<=> "foo" "bar"))}


@defform[(compare-infix comparator-expr comparison-chain)
         #:grammar
         [(comparison-chain
           (code:line operand-expr operator operand-expr)
           (code:line operand-expr operator comparison-chain))
          (operator < > <= >= == !=)]
         #:contracts ([comparator-expr comparator?])]{
 Compares the given @racket[operand-expr]s using @racket[comparator-expr], returning @racket[#true] if
 all of the specified comparisons are true and returning @racket[#false] otherwise. Unlike
 @racket[compare], this form is a macro and allows specifying comparisons with an infix syntax. The
 given @racket[operand-expr]s are evaluated in left-to-right order with short-circuiting. Each
 @racket[operand-expr] is evaluated at most once.

 @(examples
   #:eval (make-evaluator) #:once
   (compare-infix real<=> 1 < 3)
   (compare-infix real<=> 1 < 3 < 5)
   (compare-infix real<=> 100 < 3 < (error "short circuits before reaching here")))}


@defproc[(comparator-min [comparator comparator?] [v any/c] ...+) any/c]{
 Returns the smallest @racket[v], according to @racket[comparator].

 @(examples
   #:eval (make-evaluator) #:once
   (comparator-min real<=> 5 8)
   (comparator-min string<=> "foo" "bar"))}


@defproc[(comparator-max [comparator comparator?] [v any/c] ...+) any/c]{
 Returns the largest @racket[v], according to @racket[comparator].

 @(examples
   #:eval (make-evaluator) #:once
   (comparator-max real<=> 5 8)
   (comparator-max string<=> "foo" "bar"))}


@section{Constructing Comparators}

@defproc[(comparator-of-constants [constant any/c] ...) comparator?]{
 Constructs a @tech{comparator} that compares only the given @racket[constant]s using @racket[equal?].
 The order the constants are given in is interpreted as ascending order. Each @racket[constant] must
 be unique, otherwise a contract exception is raised.

 @(examples
   #:eval (make-evaluator)
   (eval:no-prompt (define size<=> (comparator-of-constants 'small 'medium 'large)))
   (compare size<=> 'small 'large)
   (compare size<=> 'medium 'medium)
   (eval:error (compare size<=> 'small 'big)))}

@defproc[(comparator-map [comparator comparator?]
                         [f (-> any/c any/c)]
                         [#:name name (or/c interned-symbol? #false) #false])
         comparator?]{
 Wraps @racket[comparator] as a @tech{comparator} that first calls @racket[f] on
 both of its inputs before comparing them. Beware that this often creates a
 comparator that is @tech{inconsistent with equality}.

 @(examples
   #:eval (make-evaluator) #:once
   (define-record-type circle (color radius))
   (define circle<=> (comparator-map real<=> circle-radius))
   (compare circle<=>
            (circle #:color 'green #:radius 5)
            (circle #:color 'blue #:radius 8)))}

@defproc[(make-comparator [function (-> any/c any/c comparison?)]
                          [#:name name (or/c interned-symbol? #false) #false])
         comparator?]{
 Constructs a @tech{comparator} named @racket[name] that compares values by
 calling @racket[function]. Most users should use @racket[comparator-map] to
 wrap an existing comparator instead of constructing one directly.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define symbol<=>
      (make-comparator
       (λ (left right)
         (cond [(symbol<? left right) lesser]
               [(equal? left right) equivalent]
               [else greater]))
       #:name 'symbol<=>)))
   (compare symbol<=> 'apple 'banana)
   (compare symbol<=> 'apple 'aardvark))}

@defproc[(comparator-reverse [comparator comparator?]) comparator?]{
 Reverses @racket[comparator], returning a comparator that considers one value
 greater than another when @racket[comparator] would consider it lesser and
 vice-versa.

 @(examples
   #:eval (make-evaluator) #:once
   (compare real<=> 2 5)
   (compare (comparator-reverse real<=>) 2 5))}

@defproc[(comparator-chain
          [comparator comparator?] ...+ [#:name name (or/c interned-symbol? #false) #false])
         comparator?]{
 Chains each @racket[comparator] together into a single comparator named @racket[name] that compares
 values using the leftmost @racket[comparator] first, with each next comparator used to break ties in
 the previous comparator.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define-enum-type gem-type (opal ruby diamond))
    (define-tuple-type gemstone (type weight))
    (define gemstone-by-type<=>
      (comparator-map (comparator-of-constants opal ruby diamond) gemstone-type))
    (define gemstone-by-weight<=> (comparator-map real<=> gemstone-weight)))

   (transduce (list (gemstone diamond 3)
                    (gemstone ruby 5)
                    (gemstone diamond 8)
                    (gemstone opal 2)
                    (gemstone ruby 14)
                    (gemstone opal 12))
              (sorting (comparator-chain gemstone-by-type<=> gemstone-by-weight<=>))
              #:into into-list))}


@defproc[(lexicographic-comparator [element-comparator comparator?])
         (comparator/c (sequence/c any/c))]{
 Constructs a @tech{comparator} of
 @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{sequences} which compares sequences in
 @hyperlink[lexicographic-order-url]{lexicographic order} by comparing each sequence element with
 @racket[element-comparator].

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define real-seq<=> (lexicographic-comparator real<=>)))
   (compare real-seq<=> (list 1 2 3) (list 3 2 1))
   (compare real-seq<=> (list 1 2 3) (list 1))
   (compare real-seq<=> (list 1 2 3) (list 2))
   (compare real-seq<=> (list 1 2 3) (list 1 2 3))
   (compare real-seq<=> (list 1 2 3) (list 1 2 3 0)))

 The two sequences need not be of the same type: they will be considered equivalent if they have
 equivalent elements in the same order. This makes this comparator @tech{inconsistent with equality}
 when two sequences are not @racket[equal?] but contain equal elements in the same order.

 @(examples
   #:eval (make-evaluator) #:once
   (equal? (list 1 2 3) (vector 1 2 3))
   (compare (lexicographic-comparator real<=>) (list 1 2 3) (vector 1 2 3)))}


@section{Predefined Comparators}

@defthing[real<=> (comparator/c comparable-real?)]{
 A @tech{comparator} that compares real numbers. Note that not all values that
 satisfy the @racket[real?] predicate can be compared: the not-a-number
 constants @racket[+nan.0] and @racket[+nan.f] are disallowed.

 @(examples
   #:eval (make-evaluator) #:once
   (compare real<=> 42 99.99)
   (compare real<=> 42 +inf.0)
   (compare real<=> 42 -inf.0)
   (eval:error (compare real<=> 42 +nan.0)))

 Beware that this comparator is @tech{inconsistent with equality}, as it ignores
 the exactness of the compared numbers. This is the same behavior as @racket[<],
 @racket[=], and @racket[>], but it means that two un-@racket[equal?] numbers
 may compare equivalent.

 @(examples
   #:eval (make-evaluator) #:once
   (compare real<=> 5 5.0)
   (compare real<=> -0.0 0.0)
   (compare real<=> +inf.0 +inf.f))}

@defproc[(comparable-real? [v any/c]) boolean?]{
 A predicate that identifies @tech/reference{real numbers} that can be compared
 sensibly. This predicate is almost identical to @racket[real?], with the
 exception that it rejects the not-a-number constants.

 @(examples
   #:eval (make-evaluator) #:once
   (comparable-real? 42)
   (comparable-real? +inf.0)
   (comparable-real? +nan.0))}

@defthing[natural<=> (comparator/c natural?)]{
 A @tech{comparator} that compares natural numbers. (A natural number is an exact, nonnegative
 integer.)

 @(examples
   #:eval (make-evaluator) #:once
   (compare natural<=> 42 100)
   (compare natural<=> 42 0)
   (compare natural<=> 42 42)
   (eval:error (compare natural<=> 42 100.0))
   (eval:error (compare natural<=> 42 -10)))}

@defthing[string<=> (comparator/c immutable-string?)]{
 A @tech{comparator} that @hyperlink[lexicographic-order-url]{lexicographically} compares immutable
 strings. Mutable strings are disallowed, to prevent clients from concurrently mutating a string
 while it's being compared.

 @(examples
   #:eval (make-evaluator) #:once
   (compare string<=> "aardvark" "zebra")
   (eval:error (compare string<=> "aardvark" (make-string 5 #\z))))}

@defthing[char<=> (comparator/c char?)]{
 A @tech{comparator} that compares characters. Comparisons are consistent with the order imposed by
 @racket[char<?].

 @(examples
   #:eval (make-evaluator) #:once
   (compare char<=> #\a #\z))}

@defthing[symbol<=> (comparator/c symbol?)]{
 A @tech{comparator} that @hyperlink[lexicographic-order-url]{lexicographically} compares symbols.
 Symbols are equivalent if they contain the same characters. Note that this comparator is
 @tech{inconsistent with equality}, because symbols that print the same are not necessarily equal, due
 to the existence of unreadable and uninterned symbols. If only interned symbols need to be compared,
 use @racket[interned-symbol<=>] to ensure comparisons are consistent with equality.

 @(examples
   #:eval (make-evaluator) #:once
   (compare symbol<=> 'aardvark 'zebra)
   (compare symbol<=> 'aardvark 'aardvark)
   (compare symbol<=> 'aardvark (string->uninterned-symbol "aardvark")))}

@defthing[interned-symbol<=> (comparator/c interned-symbol?)]{
 A @tech{comparator} that lexicographically compares interned symbols. Unreadable symbols and
 uninterned symbols are disallowed to ensure the comparator is @tech{consistent with equality} without
 imposing an arbitrary order between unequal symbols that print the same.

 @(examples
   #:eval (make-evaluator) #:once
   (compare interned-symbol<=> 'aardvark 'zebra)
   (eval:error (compare interned-symbol<=> 'aardvark (gensym 'zebra))))}

@section{Comparison Constants}

@defproc[(comparison? [v any/c]) boolean?]{
 A predicate for comparison constants.}

@defthing[lesser comparison?]{
 A comparison constant indicating that the left value of a comparison is smaller
 than the right value.}

@defthing[greater comparison?]{
 A comparison constant indicating that the left value of a comparison is greater
 than the right value.}

@defthing[equivalent comparison?]{
 A comparison constant indicating that the left value of a comparison is
 equivalent to the right value. Note that equivalent values may not be @racket[
 equal?], depending on the @tech{comparator} used.}

@section{Comparator Contracts}


@defproc[(comparator/c [operand-contract contract?]) contract?]{

 A @tech/reference{contract combinator} for @tech{comparators}. Returns a
 contract that enforces that the contracted value is a comparator, and wraps the
 comparator to check every value it compares with @racket[operand-contract]. If
 @racket[operand-contract] is a @tech/reference{chaperone contract}, then the
 returned contract is as well.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define/contract even-integer<=>
      (comparator/c (and/c integer? even?))
      real<=>))
   (compare even-integer<=> 2 8)
   (eval:error (compare even-integer<=> 3 8)))}


@defproc[(comparator-operand-contract [comparator comparator?]) contract?]{

 Returns the @tech/reference{contract} that guards values compared by @racket[comparator], or
 @racket[any/c] if @racket[comparator] does not have a contract attached to it. Note that the entire
 contract on @racket[comparator] can be retrieved with @racket[value-contract].

 @bold{Warning:} because this function observes whether a contract is attached to @racket[comparator],
 it may return different results for two comparators that are otherwise @racket[equal?]. The result
 from @racket[comparator-operand-contract] should be viewed as a best-effort optimistic estimate:
 any input that does @emph{not} satisfy the returned contract will definitely raise an error when
 given to the contracted comparator, but there is no guarantee that inputs that satisfy the returned
 contract will be accepted.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define/contract even-integer<=>
      (comparator/c (and/c integer? even?))
      real<=>))
   (comparator-operand-contract even-integer<=>)
   (value-contract even-integer<=>))}


@section{Comparator Chaperones and Impersonators}

@defproc[(comparator-impersonate
          [comparator comparator?]
          [#:operand-guard operand-guard (or/c (-> any/c any/c #false)) #false]
          [#:properties properties
           (hash/c impersonator-property? any/c #:immutable #true)
           empty-hash]
          [#:comparison-marks marks immutable-hash? empty-hash]
          [#:chaperone? chaperone? boolean? (false? operand-guard)])
         comparator?]{
 Returns an @tech/reference{impersonator} of @racket[comparator]. Whenever the
 impersonator comparator is used to compare two values, if @racket[
 operand-guard] is not false, it is applied once to each value. The application
 of the comparator's comparison function to those two values is wrapped in the
 @tech/reference{continuation marks} given by @racket[marks]. Additionally, the
 returned impersonator includes an @tech/reference{impersonator property} for
 each property and value entry within @racket[properties]. The returned
 impersonator is @racket[equal?] to @racket[comparator].

 If @racket[chaperone?] is true, the returned impersonator is a @tech/reference{
  chaperone}. In that case, @racket[operand-guard] must always return a value
 equal to the one it is given. Furthermore, any impersonators returned from
 @racket[operand-guard] must be chaperones.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define printing-real<=>
      (comparator-impersonate real<=>
                              #:operand-guard (λ (x) (printf "Got ~a\n" x) x)
                              #:chaperone? #true)))

   (compare printing-real<=> 4 8)
   (chaperone-of? printing-real<=> real<=>))}
