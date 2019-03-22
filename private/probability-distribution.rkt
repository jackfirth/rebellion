#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [probability-distribution? (-> any/c boolean?)]
  [make-probability-distribution
   (->* ((-> any/c (between/c 0 1))
         (-> any/c any/c boolean?))
        (#:name (or/c symbol? #f))
        probability-distribution?)]
  [probability-distribution-choose
   (-> probability-distribution?
       list?
       variant?)]))

(require racket/list
         racket/pretty
         racket/vector
         rebellion/tuple-type-definition
         rebellion/custom-write/tuple
         rebellion/variant)

;@------------------------------------------------------------------------------

(define-tuple-type probability-distribution (name function order)
  #:constructor plain-make-probability-distribution
  #:property-maker
  (λ (descriptor)
    (list (cons prop:object-name 0)
          (cons prop:custom-write
                (make-tuple-named-object-custom-write descriptor)))))

(define (make-probability-distribution function order #:name [name #f])
  (plain-make-probability-distribution name function order))

(define (probability-distribution-choose distribution vs)
  (define order (probability-distribution-order distribution))
  (define function (probability-distribution-function distribution))
  (define sorted-vs (sort vs order))
  (define choice (random))
  (let loop ([sorted-vs sorted-vs])
    (cond
      [(empty? sorted-vs) (variant #:rejection #f)]
      [else
       (define v (first sorted-vs))
       (define cumulative-probability (function v))
       (if (< choice cumulative-probability)
           (variant #:selection v)
           (loop (rest sorted-vs)))])))

(define (make-uniform-discrete-distribution #:order order . vs)
  (define vec (vector->immutable-vector (list->vector (sort vs order))))
  (define size (vector-length vec))
  (define cumulative-probabilities
    (vector->immutable-vector
     (for/vector #:length size
       ([v (in-vector vec)]
        [i (in-naturals 1)])
       (/ i size))))
  (define (cumulative-probability v)
    (define pos (vector-member v vec))
    (if pos
        (vector-ref cumulative-probabilities pos)
        0))
  (make-probability-distribution cumulative-probability order
                                 #:name 'uniform-discrete-distribution))

(module+ main
  (define vowels-dist
    (make-uniform-discrete-distribution #\a #\e #\i #\o #\u #:order char<?))
  (pretty-print
   (for/fold ([h (hash)])
             ([i (in-range 1000)])
     (define choice
       (probability-distribution-choose vowels-dist (list #\a #\u #\z)))
     (hash-update h choice add1 0))))
