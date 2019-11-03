#lang racket/base

(require racket/contract/base
         (only-in racket/sequence sequence/c))

(provide
 (contract-out
  [byte (-> bit? bit? bit? bit? bit? bit? bit? bit? byte?)]
  [byte-clear-leftmost-bits (-> byte? (integer-in 0 8) byte?)]
  [byte-clear-rightmost-bits (-> byte? (integer-in 0 8) byte?)]
  [byte-drop-rightmost-bits (-> byte? (integer-in 0 8) byte?)]
  [byte-ref (-> byte? (integer-in 0 7) bit?)]
  [byte-and (-> byte? byte? byte?)]
  [byte-or (-> byte? byte? byte?)]
  [byte-not (-> byte? byte?)]
  [byte-xor (-> byte? byte? byte?)]
  [byte-nand (-> byte? byte? byte?)]
  [byte-nor (-> byte? byte? byte?)]
  [byte-xnor (-> byte? byte? byte?)]
  [in-byte (-> byte? (sequence/c #:min-count 8 bit?))]
  [into-byte reducer?]
  [byte-hamming-weight (-> byte? (integer-in 0 8))]))

(require rebellion/binary/bit
         rebellion/streaming/reducer)

(module+ test
  (require (submod "..")
           rackunit
           rebellion/private/static-name))

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
  (test-case (name-string byte)
    (check-equal? (byte 0 0 0 0 0 0 0 0) 0)
    (check-equal? (byte 1 1 1 1 1 1 1 1) 255)
    (check-equal? (byte 1 0 0 0 0 0 0 0) 128)
    (check-equal? (byte 0 0 0 0 0 0 0 1) 1)
    (check-equal? (byte 0 0 0 0 1 1 0 0) 12)))

(define (byte-drop-rightmost-bits b num-bits)
  (quotient b (expt 2 num-bits)))

(module+ test
  (test-case (name-string byte-drop-rightmost-bits)
    (define f byte-drop-rightmost-bits)
    (check-equal? (f (byte 1 1 0 0 1 0 1 1) 6) (byte 0 0 0 0 0 0 1 1))
    (check-equal? (f (byte 1 1 0 0 1 0 1 1) 3) (byte 0 0 0 1 1 0 0 1))
    (check-equal? (f (byte 1 1 0 0 1 0 1 1) 0) (byte 1 1 0 0 1 0 1 1))
    (check-equal? (f (byte 1 1 0 0 1 0 1 1) 8) (byte 0 0 0 0 0 0 0 0))))

(define (byte-clear-leftmost-bits b num-bits)
  (modulo b (expt 2 (- 8 num-bits))))

(module+ test
  (test-case (name-string byte-clear-leftmost-bits)
    (define f byte-clear-leftmost-bits)
    (check-equal? (f (byte 1 1 0 0 1 0 1 1) 6) (byte 0 0 0 0 0 0 1 1))
    (check-equal? (f (byte 1 1 0 0 1 0 1 1) 3) (byte 0 0 0 0 1 0 1 1))
    (check-equal? (f (byte 1 1 0 0 1 0 1 1) 0) (byte 1 1 0 0 1 0 1 1))
    (check-equal? (f (byte 1 1 0 0 1 0 1 1) 8) (byte 0 0 0 0 0 0 0 0))))

(define (byte-clear-rightmost-bits b num-bits)
  (define n (expt 2 num-bits))
  (* (quotient b n) n))

(module+ test
  (test-case (name-string byte-clear-rightmost-bits)
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
  (test-case (name-string byte-ref)

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

(define (byte-and left right)
  (for/reducer into-byte ([i (in-range 8)])
    (bitwise-and (byte-ref left i) (byte-ref right i))))

(module+ test
  (test-case (name-string byte-and)
    (define test-patterns
      (list (byte 0 1 1 1 1 1 1 1)
            (byte 1 0 1 1 1 1 1 1)
            (byte 1 1 0 1 1 1 1 1)
            (byte 1 1 1 0 1 1 1 1)
            (byte 1 1 1 1 0 1 1 1)
            (byte 1 1 1 1 1 0 1 1)
            (byte 1 1 1 1 1 1 0 1)
            (byte 1 1 1 1 1 1 1 0)))
    (for ([x (in-range 256)]
          #:when #t
          [pattern (in-list test-patterns)]
          [i (in-range 8)])
      (check-equal? (byte-ref (byte-and x pattern) i) 0))
    (for* ([x (in-range 256)] [y (in-range 256)])
      (check-equal? (byte-and x y) (byte-and y x)))))

(define (byte-or left right)
  (for/reducer into-byte ([i (in-range 8)])
    (bitwise-ior (byte-ref left i) (byte-ref right i))))

(module+ test
  (test-case (name-string byte-or)
    (define test-patterns
      (list (byte 1 0 0 0 0 0 0 0)
            (byte 0 1 0 0 0 0 0 0)
            (byte 0 0 1 0 0 0 0 0)
            (byte 0 0 0 1 0 0 0 0)
            (byte 0 0 0 0 1 0 0 0)
            (byte 0 0 0 0 0 1 0 0)
            (byte 0 0 0 0 0 0 1 0)
            (byte 0 0 0 0 0 0 0 1)))
    (for ([x (in-range 256)]
          #:when #t
          [pattern (in-list test-patterns)]
          [i (in-range 8)])
        (check-equal? (byte-ref (byte-or x pattern) i) 1)))
    (for* ([x (in-range 256)] [y (in-range 256)])
      (check-equal? (byte-or x y) (byte-or y x))))

(define (byte-not b)
  (for/reducer into-byte ([i (in-range 8)])
    (bitwise-xor (byte-ref b i) 1)))

(module+ test
  (test-case (name-string byte-not)
    (for ([x (in-range 256)])
      (check-equal? (byte-ref (byte-not x) 0) (bitwise-xor (byte-ref x 0) 1))
      (check-equal? (byte-ref (byte-not x) 1) (bitwise-xor (byte-ref x 1) 1))
      (check-equal? (byte-ref (byte-not x) 2) (bitwise-xor (byte-ref x 2) 1))
      (check-equal? (byte-ref (byte-not x) 3) (bitwise-xor (byte-ref x 3) 1))
      (check-equal? (byte-ref (byte-not x) 4) (bitwise-xor (byte-ref x 4) 1))
      (check-equal? (byte-ref (byte-not x) 5) (bitwise-xor (byte-ref x 5) 1))
      (check-equal? (byte-ref (byte-not x) 6) (bitwise-xor (byte-ref x 6) 1))
      (check-equal? (byte-ref (byte-not x) 7) (bitwise-xor (byte-ref x 7) 1))

      (check-equal? (+ x (byte-not x)) 255))))

(define (byte-xor left right)
  (for/reducer into-byte ([i (in-range 8)])
    (bitwise-xor (byte-ref left i) (byte-ref right i))))

(module+ test
  (test-case (name-string byte-xor)
    (for ([x (in-range 256)])
      (check-equal? (byte-xor x 0) x)
      (check-equal? (byte-xor x 255) (byte-not x))
      (check-equal? (byte-xor x x) 0))))

(define (byte-nand left right)
  (for/reducer into-byte ([i (in-range 8)])
    (bitwise-xor (bitwise-and (byte-ref left i) (byte-ref right i)) 1)))

(module+ test
  (test-case (name-string byte-nand)
    (define test-patterns
      (list (byte 0 1 1 1 1 1 1 1)
            (byte 1 0 1 1 1 1 1 1)
            (byte 1 1 0 1 1 1 1 1)
            (byte 1 1 1 0 1 1 1 1)
            (byte 1 1 1 1 0 1 1 1)
            (byte 1 1 1 1 1 0 1 1)
            (byte 1 1 1 1 1 1 0 1)
            (byte 1 1 1 1 1 1 1 0)))
    (for ([x (in-range 256)]
          #:when #t
          [pattern (in-list test-patterns)]
          [i (in-range 8)])
      (check-equal? (byte-ref (byte-nand x pattern) i) 1))
    (for* ([x (in-range 256)] [y (in-range 256)])
      (check-equal? (byte-nand x y) (byte-nand y x)))))

(define (byte-nor left right)
  (for/reducer into-byte ([i (in-range 8)])
    (bitwise-xor (bitwise-ior (byte-ref left i) (byte-ref right i)) 1)))

(module+ test
  (test-case (name-string byte-nor)
    (define test-patterns
      (list (byte 1 0 0 0 0 0 0 0)
            (byte 0 1 0 0 0 0 0 0)
            (byte 0 0 1 0 0 0 0 0)
            (byte 0 0 0 1 0 0 0 0)
            (byte 0 0 0 0 1 0 0 0)
            (byte 0 0 0 0 0 1 0 0)
            (byte 0 0 0 0 0 0 1 0)
            (byte 0 0 0 0 0 0 0 1)))
    (for ([x (in-range 256)]
          #:when #t
          [pattern (in-list test-patterns)]
          [i (in-range 8)])
      (check-equal? (byte-ref (byte-nor x pattern) i) 0))
    (for* ([x (in-range 256)] [y (in-range 256)])
      (check-equal? (byte-nor x y) (byte-nor y x)))))

(define (byte-xnor left right)
  (for/reducer into-byte ([i (in-range 8)])
    (bitwise-xor (bitwise-xor (byte-ref left i) (byte-ref right i)) 1)))

(module+ test
  (test-case (name-string byte-xnor)
    (for ([x (in-range 256)])
      (check-equal? (byte-xnor x 0) (byte-not x))
      (check-equal? (byte-xnor x 255) x)
      (check-equal? (byte-xnor x x) 255))))

(define (in-byte b)
  (for/list ([n (in-range 8)])
    (byte-ref b n)))

(define into-byte
  (reducer-limit (make-fold-reducer (λ (st b) (+ (* st 2) b)) 0
                                    #:name 'into-byte)
                 8))

(module+ test
  (test-case (name-string in-byte)
    (for ([x (in-range 256)]
          #:when #t
          [b (in-byte x)]
          [i (in-range 8)])
      (check-equal? b (byte-ref x i))))
  (test-case (name-string into-byte)
    (check-equal? (reduce into-byte 0 0 0 0 0 0 0 0) 0)
    (check-equal? (reduce into-byte 1 1 1 1 1 1 1 1) 255)
    (check-equal? (reduce into-byte 0 0 0 0 0 0 0 1) 1)
    (check-equal? (reduce into-byte 0 0 0 0 0 1 0 0) 4)
    (check-equal? (reduce into-byte 0 0 0 0 0 1 0 1) 5)
    (check-equal? (reduce into-byte 1 0 0 0 0 0 0 0) 128)
    (check-equal? (reduce into-byte 1 0 1 1 0 1 0 0) 180)))

(define (byte-hamming-weight b)
  (for/sum ([bit (in-byte b)]) bit))

(module+ test
  (test-case (name-string byte-hamming-weight)
    (for ([x (in-range 256)])
      (check-equal? (byte-hamming-weight x)
                    (- 8 (byte-hamming-weight (byte-not x)))))
    (for ([position (in-range 8)])
      (check-equal? (byte-hamming-weight (arithmetic-shift 1 position)) 1))
    (check-equal? (byte-hamming-weight 0) 0)
    (check-equal? (byte-hamming-weight 255) 8)
    (check-equal? (byte-hamming-weight (byte 1 0 1 0 1 1 1 0)) 5)))
