#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [make-equal+hash-property
   (-> natural? (-> any/c natural? any/c)
       (list/c procedure? procedure? procedure?))]
  [make-struct-equal+hash-property
   (-> uninitialized-struct-descriptor?
       (list/c procedure? procedure? procedure?))]))

(require racket/math
         rebellion/generative-token
         rebellion/struct-descriptor)

;@------------------------------------------------------------------------------

(define (make-equal+hash-property size accessor)
  (define token (make-generative-token))
  (define (equal-proc this other recur)
    (for/and ([pos (in-range size)])
      (recur (accessor this pos) (accessor other pos))))
  (define (hash-proc this recur)
    (let loop ([pos size] [previous-vs (list token)])
      (cond
        [(zero? pos) (recur previous-vs)]
        [else
         (define next-pos (sub1 pos))
         (loop next-pos (cons (accessor this next-pos) previous-vs))])))
  (define hash2-proc hash-proc)
  (list equal-proc hash-proc hash2-proc))
  
(define (make-struct-equal+hash-property descriptor)
  (define accessor (struct-descriptor-accessor descriptor))
  (define size
    (+ (struct-descriptor-mutable-fields descriptor)
       (struct-descriptor-immutable-fields descriptor)
       (struct-descriptor-auto-fields descriptor)))
  (make-equal+hash-property size accessor))
