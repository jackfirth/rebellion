#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [make-accessor-based-equal+hash
   (-> (-> any/c natural? any/c) natural?
       (list/c procedure? procedure? procedure?))]))

(require racket/math
         rebellion/generative-token)

;@------------------------------------------------------------------------------

(define (make-accessor-based-equal+hash accessor size)
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
