#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [immutable-vector? predicate/c]
  [make-immutable-vector (-> natural? any/c immutable-vector?)]
  [immutable-vector (-> any/c ... immutable-vector?)]
  [immutable-vector-length (-> immutable-vector? natural?)]
  [immutable-vector-ref (-> immutable-vector? natural? any/c)]
  [immutable-vector->list (-> immutable-vector? list?)]
  [list->immutable-vector (-> list? immutable-vector?)]
  [immutable-vector->values
   (->* (immutable-vector?) (natural? natural?) any)]
  [build-immutable-vector (-> natural? (-> natural? any/c) immutable-vector?)]
  [immutable-vector-map
   (-> procedure? immutable-vector? immutable-vector? ... immutable-vector?)]
  [immutable-vector-append (-> immutable-vector? ... immutable-vector?)]
  [immutable-vector-take (-> immutable-vector? natural? immutable-vector?)]
  [immutable-vector-take-right
   (-> immutable-vector? natural? immutable-vector?)]
  [immutable-vector-drop (-> immutable-vector? natural? immutable-vector?)]
  [immutable-vector-drop-right
   (-> immutable-vector? natural? immutable-vector?)]
  [immutable-vector-split-at
   (-> immutable-vector? natural? (values immutable-vector? immutable-vector?))]
  [immutable-vector-split-at-right
   (-> immutable-vector? natural? (values immutable-vector? immutable-vector?))]
  [immutable-vector-copy
   (-> immutable-vector? natural? natural? immutable-vector?)]
  [immutable-vector-filter (-> predicate/c immutable-vector? immutable-vector?)]
  [immutable-vector-filter-not
   (-> predicate/c immutable-vector? immutable-vector?)]
  [immutable-vector-count
   (-> (-> any/c any/c) immutable-vector? immutable-vector? ... natural?)]
  [immutable-vector-argmin (-> (-> any/c real?) immutable-vector? any/c)]
  [immutable-vector-argmax (-> (-> any/c real?) immutable-vector? any/c)]
  [immutable-vector-member (-> any/c immutable-vector? (or/c natural? #f))]
  [immutable-vector-memv (-> any/c immutable-vector? (or/c natural? #f))]
  [immutable-vector-memq (-> any/c immutable-vector? (or/c natural? #f))]
  [immutable-vector-sort
   (->* (immutable-vector? (-> any/c any/c boolean?))
        (natural? natural? #:key (-> any/c any/c) #:cache-keys? boolean?)
        immutable-vector?)]
  [empty-immutable-vector empty-immutable-vector?]
  [empty-immutable-vector? predicate/c]
  [nonempty-immutable-vector? predicate/c]))

(require (for-syntax racket/base
                     racket/string)
         racket/math
         racket/vector
         syntax/parse/define)

;@------------------------------------------------------------------------------

(define (immutable-vector? v) (and (vector? v) (immutable? v)))

(define empty-immutable-vector #())

(define (empty-immutable-vector? v)
  (and (equal? v empty-immutable-vector) (immutable? v)))

(define (nonempty-immutable-vector? v)
  (and (immutable-vector? v) (not (equal? v empty-immutable-vector))))

;@------------------------------------------------------------------------------

(define immutable-vector-length vector-length)
(define immutable-vector-ref vector-ref)
(define immutable-vector->list vector->list)
(define immutable-vector->values vector->values)
(define immutable-vector-count vector-count)
(define immutable-vector-argmin vector-argmin)
(define immutable-vector-argmax vector-argmax)
(define immutable-vector-member vector-member)
(define immutable-vector-memv vector-memv)
(define immutable-vector-memq vector-memq)

;@------------------------------------------------------------------------------

(begin-for-syntax
  (define-syntax-class positional-argument
    (pattern id:id)
    (pattern [id:id default:expr]))

  (define-syntax-class vector-function-header
    #:attributes (id lambda-argument-binders function-call-expression)

    (pattern (id:id arg:positional-argument ...)
      #:with lambda-argument-binders #'(arg ...)
      #:with function-call-expression #'(id arg.id ...))

    (pattern (id:id . rest-arg:id)
      #:with lambda-argument-binders #'rest-arg
      #:with function-call-expression #'(apply id rest-arg))

    (pattern (id:id arg:positional-argument ...+ . rest-arg:id)
      #:with lambda-argument-binders #'(arg ... . rest-arg)
      #:with function-call-expression #'(apply id arg.id ... rest-arg))))

(define-simple-macro
  (define-wrapper header:vector-function-header)
  #:do [(define id-str (symbol->string (syntax-e #'header.id)))
        (define imm-str (string-replace id-str "vector" "immutable-vector"))]
  #:with immutable-id (datum->syntax #'header.id (string->symbol imm-str))
  (define immutable-id
    (λ header.lambda-argument-binders
      (vector->immutable-vector header.function-call-expression))))

;@------------------------------------------------------------------------------

(define-wrapper (make-vector size v))
(define-wrapper (vector . vs))
(define-wrapper (list->vector lst))
(define-wrapper (build-vector size f))
(define-wrapper (vector-map f . vecs))
(define-wrapper (vector-append . vecs))
(define-wrapper (vector-take vec n))
(define-wrapper (vector-take-right vec n))
(define-wrapper (vector-drop vec n))
(define-wrapper (vector-drop-right vec n))
(define-wrapper (vector-copy vec start end))
(define-wrapper (vector-filter pred vec))
(define-wrapper (vector-filter-not pred vec))

;@------------------------------------------------------------------------------

(define (immutable-vector-split-at vec pos)
  (define-values (before after) (vector-split-at vec pos))
  (values (vector->immutable-vector before)
          (vector->immutable-vector after)))

(define (immutable-vector-split-at-right vec pos)
  (define-values (before after) (vector-split-at-right vec pos))
  (values (vector->immutable-vector before)
          (vector->immutable-vector after)))

(define (immutable-vector-sort vec
                               less-than?
                               [start 0]
                               [end (immutable-vector-length vec)]
                               #:key [key-function values]
                               #:cache-keys? [cache-keys? #f])
  (vector-sort vec less-than? start end
               #:key key-function
               #:cache-keys? cache-keys?))
