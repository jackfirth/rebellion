#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [make-accessor-based-equal+hash
   (-> (-> any/c natural? any/c) natural? equal+hash/c)]
  [make-singleton-equal+hash (-> equal+hash/c)]
  [equal+hash/c contract?]))

(require racket/math
         rebellion/generative-token)

;@------------------------------------------------------------------------------

(define equal+hash/c (list/c procedure? procedure? procedure?))

(define (make-accessor-based-equal+hash accessor size)
  (define token (make-generative-token))
  (define (equal-proc this other recur)
    (for/and ([pos (in-range size)])
      (recur (accessor this pos) (accessor other pos))))
  (define (hash-proc this recur)
    (recur (cons token (build-list size (λ (pos) (accessor this pos))))))
  (define hash2-proc hash-proc)
  (list equal-proc hash-proc hash2-proc))

(define (make-singleton-equal+hash)
  (define token (make-generative-token))
  (define (equal-proc this other recur) #t)
  (define (hash-proc _ recur) (recur token))
  (define hash2-proc hash-proc)
  (list equal-proc hash-proc hash2-proc))
