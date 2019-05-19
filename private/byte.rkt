#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [byte (-> bit? bit? bit? bit? bit? bit? bit? bit? byte?)]
  [byte-clear-leftmost-bits (-> byte? (integer-in 0 8) byte?)]
  [byte-clear-rightmost-bits (-> byte? (integer-in 0 8) byte?)]
  [byte-drop-rightmost-bits (-> byte? (integer-in 0 8) byte?)]
  [byte-ref (-> byte? (integer-in 0 7) bit?)]))

(require rebellion/binary/bit)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define (byte a b c d e f g h)
  (+ (* a 128)
     (* b 64)
     (* c 32)
     (* d 16)
     (* e 8)
     (* f 4)
     (* g 2)
     h))

(module+ test
  (test-case "byte"
    (check-equal? (byte 0 0 0 0 0 0 0 0) 0)
    (check-equal? (byte 1 1 1 1 1 1 1 1) 255)
    (check-equal? (byte 1 0 0 0 0 0 0 0) 128)
    (check-equal? (byte 0 0 0 0 0 0 0 1) 1)
    (check-equal? (byte 0 0 0 0 1 1 0 0) 12)))

(define (byte-drop-rightmost-bits b num-bits)
  (quotient b (expt 2 num-bits)))

(module+ test
  (test-case "byte-drop-rightmost-bits"
    (define f byte-drop-rightmost-bits)
    (check-equal? (f (byte 1 1 0 0 1 0 1 1) 6) (byte 0 0 0 0 0 0 1 1))
    (check-equal? (f (byte 1 1 0 0 1 0 1 1) 3) (byte 0 0 0 1 1 0 0 1))
    (check-equal? (f (byte 1 1 0 0 1 0 1 1) 0) (byte 1 1 0 0 1 0 1 1))
    (check-equal? (f (byte 1 1 0 0 1 0 1 1) 8) (byte 0 0 0 0 0 0 0 0))))

(define (byte-clear-leftmost-bits b num-bits)
  (modulo b (expt 2 (- 8 num-bits))))

(module+ test
  (test-case "byte-clear-leftmost-bits"
    (define f byte-clear-leftmost-bits)
    (check-equal? (f (byte 1 1 0 0 1 0 1 1) 6) (byte 0 0 0 0 0 0 1 1))
    (check-equal? (f (byte 1 1 0 0 1 0 1 1) 3) (byte 0 0 0 0 1 0 1 1))
    (check-equal? (f (byte 1 1 0 0 1 0 1 1) 0) (byte 1 1 0 0 1 0 1 1))
    (check-equal? (f (byte 1 1 0 0 1 0 1 1) 8) (byte 0 0 0 0 0 0 0 0))))

(define (byte-clear-rightmost-bits b num-bits)
  (define n (expt 2 num-bits))
  (* (quotient b n) n))

(module+ test
  (test-case "byte-clear-rightmost-bits"
    (define f byte-clear-rightmost-bits)
    (check-equal? (f (byte 1 1 0 0 1 0 1 1) 0) (byte 1 1 0 0 1 0 1 1))
    (check-equal? (f (byte 1 1 0 0 1 0 1 1) 1) (byte 1 1 0 0 1 0 1 0))
    (check-equal? (f (byte 1 1 0 0 1 0 1 1) 2) (byte 1 1 0 0 1 0 0 0))
    (check-equal? (f (byte 1 1 0 0 1 0 1 1) 3) (byte 1 1 0 0 1 0 0 0))
    (check-equal? (f (byte 1 1 0 0 1 0 1 1) 4) (byte 1 1 0 0 0 0 0 0))
    (check-equal? (f (byte 1 1 0 0 1 0 1 1) 5) (byte 1 1 0 0 0 0 0 0))
    (check-equal? (f (byte 1 1 0 0 1 0 1 1) 6) (byte 1 1 0 0 0 0 0 0))
    (check-equal? (f (byte 1 1 0 0 1 0 1 1) 7) (byte 1 0 0 0 0 0 0 0))
    (check-equal? (f (byte 1 1 0 0 1 0 1 1) 8) (byte 0 0 0 0 0 0 0 0))))

(define (byte-ref b pos)
  (define left-bits pos)
  (define right-bits (- 7 pos))
  (byte-drop-rightmost-bits (byte-clear-leftmost-bits b left-bits) right-bits))

(module+ test
  (test-case "byte-ref"

    (test-case "edge-values"
      (for ([i (in-range 8)])
        (with-check-info (['i i])
          (check-equal? (byte-ref 0 i) 0)
          (check-equal? (byte-ref 255 i) 1))))

    (test-case "stress-test"
      (for ([_ (in-range 1000)])
        (define a (random 2))
        (define b (random 2))
        (define c (random 2))
        (define d (random 2))
        (define e (random 2))
        (define f (random 2))
        (define g (random 2))
        (define h (random 2))
        (define abcdefgh (byte a b c d e f g h))
        (with-check-info (['abcdefgh abcdefgh])
          (check-equal? (byte-ref abcdefgh 0) a)
          (check-equal? (byte-ref abcdefgh 1) b)
          (check-equal? (byte-ref abcdefgh 2) c)
          (check-equal? (byte-ref abcdefgh 3) d)
          (check-equal? (byte-ref abcdefgh 4) e)
          (check-equal? (byte-ref abcdefgh 5) f)
          (check-equal? (byte-ref abcdefgh 6) g)
          (check-equal? (byte-ref abcdefgh 7) h))))))
