#lang scribble/manual

@(require (for-label rebellion/collection)
          (submod rebellion/private/scribble-cross-document-tech doc))

@title[#:tag "choosing-collections"]{Choosing Collections}

The modules in @racketmodname[rebellion/collection] provide a wide selection of
different collections. Choosing the right one can be tricky, so this document
aims to make that decision easier. Treat this as @emph{guidance} rather than
@emph{rules} --- the lines between collection types are blurry and can shift
with circumstance.

@section{To Key or Not to Key}

The first question you should ask yourself is, do the items of this collection
have keys? Can items be looked up by key? If so, you have a @deftech{
 dictionary-like collection}, also called a @deftech{bicollection}. There are
many different kinds of bicollections that vary in whether or not keys are
unique, whether entries are unordered, ordered, or sorted, and whether any type
of value can serve as a key or only certain types of values. Bicollections can
usually be viewed as a collection of key-value @tech{entries}.

@tabular[
 #:style 'boxed
 #:column-properties
 (list (list 'center 'left-border 'right-border) (list 'center 'right-border))
 #:row-properties (list 'bottom-border '() '() '() '() 'bottom-border)
 #:cell-properties (list (list 'vcenter))
 (list (list @bold{Bicollections} @bold{Not bicollections})
       (list @tech/reference{Hash tables} @tech/reference{Lists})
       (list @tech/reference{Dictionaries} @tech/reference{Sets})
       (list @tech{Multidicts} @tech{Multisets})
       (list @tech{Association lists} @tech{Keysets})
       (list @tech{Records} @tech{Tables}))]

@section{Collection Cheat Sheet}

@tabular[
 #:style 'boxed
 (list
  (list @italic{Instead of using...} @italic{Consider using...})
  (list
   @elem{An unordered @tech/reference{list} with no duplicates}
   @elem{A @tech/reference{set}})

  (list
   @elem{An unordered @tech/reference{list}}
   @elem{A @tech{multiset}})

  (list
   @elem{A @tech/reference{hash} whose values are always @racket[#true]}
   @elem{A @tech/reference{set}})

  (list
   @elem{A @tech/reference{hash} whose values are nonempty @tech/reference{
   sets}}
   @elem{A @tech{multidict}})

  (list
   @elem{A @tech/reference{hash} whose values are nonempty @tech/reference{
   lists}}
   @elem{An @tech{association list}})

  (list
   @elem{A @tech/reference{hash} whose values are positive integers representing
  frequencies}
   @elem{A @tech{multiset}})

  (list
   @elem{An unordered @tech/reference{list} of key-value pairs with no duplicate
  keys}
   @elem{A @tech/reference{hash}})

  (list
   @elem{An unordered @tech/reference{list} of key-value pairs}
   @elem{A @tech{multidict}})

  (list
   @elem{A @tech/reference{list} of key-value pairs where order is significant}
   @elem{An @tech{association list}})

  (list
   @elem{A @tech/reference{list} or @tech/reference{set} of keywords or symbols}
   @elem{A @tech{keyset}})

  (list
   @elem{A @tech/reference{hash} whose keys are symbols or strings}
   @elem{A @tech{record}})

  (list
   @elem{A @tech/reference{list} of symbol-keyed @tech/reference{hash}es}
   @elem{A @tech{table}}))]
