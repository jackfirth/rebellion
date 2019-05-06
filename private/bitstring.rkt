#lang racket/base

(require racket/contract)

;; TODO: add bitstring-ref
(provide
 (contract-out
  [bitstring (-> bit? ... bitstring?)]
  [bitstring? predicate/c]
  [bitstring->padded-bytes (-> bitstring? immutable-bytes?)]
  [bytes->bitstring (->* (immutable-bytes?)
                         (#:padding (integer-in 0 7))
                         bitstring?)]))

(require racket/list
         rebellion/bit
         rebellion/byte
         rebellion/tuple-type-definition)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

;; TODO: put this in its own module
(define (immutable-bytes? v) (and (bytes? v) (immutable? v)))

;@------------------------------------------------------------------------------

;; TODO: make bitstrings print nicely
(define-tuple-type bitstring (bytes padding) #:constructor plain-bitstring)

(define (bitstring . bits)
  (define size (length bits))
  (define-values (nonexcess-bytes-size excess-size) (quotient/remainder size 8))
  (define bytes-size
    (if (zero? excess-size) nonexcess-bytes-size (add1 nonexcess-bytes-size)))
  (define padding-size (if (zero? excess-size) 0 (- 8 excess-size)))
  (define padding-bits (make-list padding-size 0))

  ;; The approach used here combines an immutable functional fold and an
  ;; imperatively-updated mutable bytestring. This is confusing, but it
  ;; minimizes the memory allocation required to construct the padded
  ;; bytestring.
  (define mutable-padded-bytes (make-bytes bytes-size))
  (for/fold ([current-byte 0]
             [current-index 0]
             #:result (void))
            ([b (in-sequences (in-list bits) (in-list padding-bits))]
             [eighth-bit? (in-cycle (in-list (list #f #f #f #f #f #f #f #t)))])
    (define next-byte (+ b (* current-byte 2)))
    (cond [eighth-bit?
           (bytes-set! mutable-padded-bytes current-index next-byte)
           (values 0 (add1 current-index))]
          [else
           (values next-byte current-index)]))

  ;; Unfortunately Racket bytestrings do not support a freeze operation, which
  ;; would let us make a mutable bytestring immutable without allocating a copy.
  ;; This means we use double the necessary memory when constructing large
  ;; bitstrings.
  (define padded-bytes (bytes->immutable-bytes mutable-padded-bytes))

  (plain-bitstring padded-bytes padding-size))

(define (bitstring->padded-bytes bits) (bitstring-bytes bits))

(define (bytes->bitstring bytes #:padding [padding 0])
  (cond [(zero? padding) (bitstring bytes 0)]
        [else
         (define size (bytes-length bytes))
         (define last-pos (sub1 size))
         (define mutable-padded-bytes (make-bytes size 0))
         (bytes-copy! mutable-padded-bytes 0
                      bytes 0 last-pos)
         (define last-byte (bytes-ref bytes last-pos))
         (define padded-last-byte (byte-clear-rightmost-bits padding))
         (bytes-set! mutable-padded-bytes last-pos padded-last-byte)
         (define padded-bytes (bytes->immutable-bytes mutable-padded-bytes))
         (bitstring padded-bytes padding)]))

(module+ test
  (test-case "bitstring"
    (for ([i (in-range 0 100)])
      (define zero-bits (apply bitstring (make-list i 0)))
      (define one-bits (apply bitstring (make-list i 1)))
      (void))
    (test-case "roundtripping"
      (check-equal? (bitstring-bytes (bitstring 0 0 0 0 1 1 1 1
                                                1 1 0 0 1 1 0 0
                                                1 0 0 1))
                    (bytes (byte 0 0 0 0 1 1 1 1)
                           (byte 1 1 0 0 1 1 0 0)
                           (byte 1 0 0 1 0 0 0 0))))))
