#lang racket/base

(require racket/contract/base
         racket/contract/combinator)

(provide
 (contract-out
  [enum-descriptor? predicate/c]
  [initialized-enum-descriptor? predicate/c]
  [uninitialized-enum-descriptor? predicate/c]
  [make-enum-implementation
   (->* (enum-type?)
        (#:inspector inspector?
         #:property-maker (-> uninitialized-enum-descriptor? properties/c))
        initialized-enum-descriptor?)]
  [enum-descriptor-type (-> enum-descriptor? enum-type?)]
  [enum-descriptor-predicate (-> enum-descriptor? predicate/c)]
  [enum-descriptor-selector
   (->i #:chaperone
        ([descriptor enum-descriptor?])
        [_ (descriptor)
           (-> (enum-index/c descriptor)
               (enum-descriptor-predicate descriptor))])]
  [enum-descriptor-discriminator
   (->i #:chaperone
        ([descriptor enum-descriptor?])
        [_ (descriptor)
           (-> (enum-descriptor-predicate descriptor)
               (enum-index/c descriptor))])]
  [default-enum-properties (-> enum-descriptor? properties/c)]
  [default-enum-custom-write (-> enum-descriptor? custom-write-function/c)]
  [default-enum-equal+hash (-> enum-descriptor? equal+hash/c)]
  [default-enum-object-name (-> enum-descriptor? object-name/c)]
  [default-enum-flat-contract (-> enum-descriptor? flat-contract-property?)]))

(require racket/math
         rebellion/base/generative-token
         rebellion/base/immutable-string
         rebellion/collection/immutable-vector
         rebellion/collection/keyset/low-dependency
         rebellion/custom-write
         rebellion/equal+hash
         rebellion/type/enum/base
         rebellion/type/tuple)

;@------------------------------------------------------------------------------

(define properties/c (listof (cons/c struct-type-property? any/c)))

(define (write-descriptor this out _)
  (define name (object-name this))
  (write-string "#<enum-descriptor:" out)
  (write-string (symbol->string name) out)
  (write-string ">" out)
  (void))

(struct initialized-enum-descriptor
  (type predicate selector discriminator backing-tuple-descriptor)
  #:omit-define-syntaxes
  #:constructor-name constructor:initialized-enum-descriptor

  #:property prop:object-name
  (λ (this) (enum-type-name (initialized-enum-descriptor-type this)))

  #:property prop:custom-write write-descriptor
  #:property prop:custom-print-quotable 'never)

(struct uninitialized-enum-descriptor
  (type predicate selector discriminator)
  #:omit-define-syntaxes
  #:constructor-name constructor:uninitialized-enum-descriptor

  #:property prop:object-name
  (λ (this) (enum-type-name (uninitialized-enum-descriptor-type this)))

  #:property prop:custom-write write-descriptor
  #:property prop:custom-print-quotable 'never)

(define (initialized-enum-descriptor
         #:type type
         #:predicate predicate
         #:selector selector
         #:discriminator discriminator
         #:backing-tuple-descriptor tuple-descriptor)
  (constructor:initialized-enum-descriptor
   type predicate selector discriminator tuple-descriptor))

(define (uninitialized-enum-descriptor
         #:type type
         #:predicate predicate
         #:selector selector
         #:discriminator discriminator)
  (constructor:uninitialized-enum-descriptor
   type predicate selector discriminator))

(define (enum-descriptor? v)
  (or (initialized-enum-descriptor? v) (uninitialized-enum-descriptor? v)))

(define ((make-getter initialized uninitialized) descriptor)
  (if (initialized-enum-descriptor? descriptor)
      (initialized descriptor)
      (uninitialized descriptor)))

(define enum-descriptor-type
  (make-getter initialized-enum-descriptor-type
               uninitialized-enum-descriptor-type))

(define enum-descriptor-predicate
  (make-getter initialized-enum-descriptor-predicate
               uninitialized-enum-descriptor-predicate))

(define enum-descriptor-selector
  (make-getter initialized-enum-descriptor-selector
               uninitialized-enum-descriptor-selector))

(define enum-descriptor-discriminator
  (make-getter initialized-enum-descriptor-discriminator
               uninitialized-enum-descriptor-discriminator))

(define (make-enum-implementation
         type
         #:inspector [inspector (current-inspector)]
         #:property-maker [prop-maker default-enum-properties])
  (define (tuple-prop-maker descriptor)
    (prop-maker (tuple-descriptor->enum-descriptor descriptor type)))
  (define descriptor
    (make-tuple-implementation (enum-type->tuple-type type)
                               #:inspector inspector
                               #:property-maker tuple-prop-maker))
  (tuple-descriptor->enum-descriptor descriptor type))

(define (tuple-descriptor->enum-descriptor descriptor type)
  (define predicate (tuple-descriptor-predicate descriptor))
  (define selector (tuple-descriptor-constructor descriptor))
  (define tuple-accessor (tuple-descriptor-accessor descriptor))
  (define discriminator
    (procedure-rename
     (λ (instance) (tuple-accessor instance 0))
     (enum-type-discriminator-name type)))
  (if (initialized-tuple-descriptor? descriptor)
      (initialized-enum-descriptor
       #:type type
       #:predicate predicate
       #:selector selector
       #:discriminator discriminator
       #:backing-tuple-descriptor descriptor)
      (uninitialized-enum-descriptor
       #:type type
       #:predicate predicate
       #:selector selector
       #:discriminator discriminator)))

(define (enum-type->tuple-type type)
  (tuple-type (enum-type-name type) (list 'index)
              #:predicate-name (enum-type-predicate-name type)
              #:constructor-name (enum-type-selector-name type)))

(define (default-enum-properties descriptor)
  (define equal+hash (default-enum-equal+hash descriptor))
  (define custom-write (default-enum-custom-write descriptor))
  (define object-name (default-enum-object-name descriptor))
  (define flat-contract (default-enum-flat-contract descriptor))
  (list (cons prop:equal+hash equal+hash)
        (cons prop:custom-write custom-write)
        (cons prop:object-name object-name)
        (cons prop:custom-print-quotable 'never)
        (cons prop:flat-contract flat-contract)))

(define (enum-type->constant-names type
                                   [keyword->name
                                    (compose1 string->symbol keyword->immutable-string)])
  (define constants (enum-type-constants type))
  (vector->immutable-vector
   (for/vector #:length (keyset-size constants)
     ([constant (in-keyset constants)])
     (keyword->name constant))))

(define (default-enum-custom-write descriptor)
  (define type (enum-descriptor-type descriptor))
  (define name (enum-type-name type))
  (define prefix (string-append "#<" (symbol->string name) ":"))
  (define constant-strings (enum-type->constant-names type keyword->immutable-string))
  (define discriminator (enum-descriptor-discriminator descriptor))
  (λ (this out mode)
    (write-string prefix out)
    (write-string
     (immutable-vector-ref constant-strings (discriminator this)) out)
    (write-string ">" out)
    (void)))

(define (make-delegating-equal+hash delegate-extractor)
  (define token (make-generative-token))
  (define (equal-proc this other recur)
    (recur (delegate-extractor this) (delegate-extractor other)))
  (define (hash-proc this recur)
    (recur (cons token (delegate-extractor this))))
  (define hash2-proc hash-proc)
  (list equal-proc hash-proc hash2-proc))

(define (default-enum-equal+hash descriptor)
  (make-delegating-equal+hash (enum-descriptor-discriminator descriptor)))

(define (default-enum-object-name descriptor)
  (define names (enum-type->constant-names (enum-descriptor-type descriptor)))
  (define discriminator (enum-descriptor-discriminator descriptor))
  (λ (this) (immutable-vector-ref names (discriminator this))))

(define (default-enum-flat-contract descriptor)
  (build-flat-contract-property
   #:name (default-enum-object-name descriptor)
   #:first-order (λ (this) (λ (x) (eq? x this)))))

(define (enum-index/c descriptor)
  (define size (enum-type-size (enum-descriptor-type descriptor)))
  (integer-in 0 size))

(define object-name/c (or/c natural? (-> any/c any/c)))
