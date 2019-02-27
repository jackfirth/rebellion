#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [make-struct-equal+hash-property
   (-> uninitialized-struct-descriptor?
       (list/c procedure? procedure? procedure?))]))

(require rebellion/generative-token
         rebellion/struct-descriptor)

;@------------------------------------------------------------------------------

(define (make-struct-equal+hash-property descriptor)
  (define accessor (struct-descriptor-accessor descriptor))
  (define fields
    (+ (struct-descriptor-mutable-fields descriptor)
       (struct-descriptor-immutable-fields descriptor)
       (struct-descriptor-auto-fields descriptor)))
  (define token (make-generative-token))
  (define (equal-proc this other recur)
    (for/and ([field (in-range fields)])
      (recur (accessor this field) (accessor other field))))
  (define (hash-proc this recur)
    (let loop ([field fields] [previous-vs (list token)])
      (cond
        [(zero? field) (recur previous-vs)]
        [else
         (define next-field (sub1 field))
         (loop next-field (cons (accessor this next-field) previous-vs))])))
  (define hash2-proc hash-proc)
  (list equal-proc hash-proc hash2-proc))
