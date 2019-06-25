#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/random
                     rebellion/binary/base-encoding
                     rebellion/binary/immutable-bytes
                     rebellion/immutable-string)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'racket/random
                   'rebellion/binary/base-encoding)
    #:private (list 'racket/base)))

@title{Base Encoding}
@defmodule[rebellion/binary/base-encoding]

@defproc[(base64-encode [bytes immutable-bytes?]) immutable-string?]{

 @(examples
   #:eval (make-evaluator) #:once
   (base64-encode #"apple")
   (base64-encode (bytes->immutable-bytes (crypto-random-bytes 10)))
   (base64-encode (bytes->immutable-bytes (crypto-random-bytes 50))))}

@defproc[(base64url-encode [bytes immutable-bytes?]) immutable-string?]{

 @(examples
   #:eval (make-evaluator) #:once
   (base64url-encode #"apple")
   (base64url-encode (bytes->immutable-bytes (crypto-random-bytes 10)))
   (base64url-encode (bytes->immutable-bytes (crypto-random-bytes 50))))}
