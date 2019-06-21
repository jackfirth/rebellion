#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     rebellion/collection/entry
                     rebellion/collection/multidict
                     rebellion/collection/multiset)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/collection/multidict
                   'rebellion/collection/entry
                   'rebellion/collection/multiset)
    #:private (list 'racket/base)))

@title{Multidicts}
@defmodule[rebellion/collection/multidict]

@defproc[(multidict? [v any/c]) boolean?]

@defproc[(multidict [k any/c] [v any/c] ... ...) multidict?]{
 @(examples
   #:eval (make-evaluator) #:once
   (multidict 'a 1 'b 2 'c 3)
   (multidict 'a 1 'a 2 'a 3)
   (multidict 'a 1 'a 1 'b 1))}

@defproc[(multidict-ref [dict multidict?] [k any/c]) immutable-set?]{
 @(examples
   #:eval (make-evaluator) #:once
   (define dict
     (multidict 'fruit 'apple
                'fruit 'orange
                'fruit 'banana
                'vegetable 'carrot
                'vegetable 'celery))
   (multidict-ref dict 'fruit)
   (multidict-ref dict 'vegetable)
   (multidict-ref dict 'dessert))}

@defproc[(multidict-size [dict multidict?]) natural?]{
 @(examples
   #:eval (make-evaluator) #:once
   (multidict-size (multidict 'a 1 'b 2 'c 3))
   (multidict-size (multidict 'a 1 'b 2 'b 3))
   (multidict-size (multidict 'a 1 'a 1 'a 1)))}

@defproc[(multidict-keys [dict multidict?]) multiset?]{
 @(examples
   #:eval (make-evaluator) #:once
   (multidict-keys
    (multidict 'fruit 'apple
               'fruit 'orange
               'fruit 'banana
               'vegetable 'carrot
               'vegetable 'celery)))}

@defproc[(multidict-values [dict multidict?]) multiset?]{
 @(examples
   #:eval (make-evaluator) #:once
   (multidict-values
    (multidict "Iron Man" 'marvel
               "Superman" 'dc
               "The Black Panther" 'marvel
               "Wonder Woman" 'dc
               "The Hulk" 'marvel
               "Captain Marvel" 'marvel)))}

@defproc[(multidict-unique-keys [dict multidict?]) immutable-set?]{
 @(examples
   #:eval (make-evaluator) #:once
   (multidict-unique-keys
    (multidict 'fruit 'apple
               'fruit 'orange
               'fruit 'banana
               'vegetable 'carrot
               'vegetable 'celery)))}

@defproc[(multidict-entries [dict multidict?]) (immutable-set/c entry?)]

@defproc[(multidict->hash [dict multidict?])
         (hash/c any/c nonempty-immutable-set? #:immutable? #t)]

@defproc[(multidict-contains-key? [dict multidict?] [k any/c]) boolean?]

@defproc[(multidict-contains-value? [dict multidict?] [v any/c]) boolean?]

@defproc[(multidict-contains-entry? [dict multidict?] [e entry?]) boolean?]

@defthing[empty-multidict empty-multidict?]

@defproc[(empty-multidict? [v any/c]) boolean?]

@defproc[(nonempty-multidict? [v any/c]) boolean?]
