#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [bitstring (-> bit? ... bitstring?)]
  [bitstring? predicate/c]
  [bitstring-size (-> bitstring? natural?)]
  [bitstring-ref
   (->i ([bits () bitstring?]
         [pos (bits) (integer-in 0 (bitstring-size bits))])
        [_ bit?])]
  [in-bitstring (-> bitstring? (sequence/c bit?))]
  [bitstring->padded-bytes (-> bitstring? immutable-bytes?)]
  [bytes->bitstring
   (->* (immutable-bytes?) (#:padding (integer-in 0 7)) bitstring?)]
  [empty-bitstring bitstring?]))

(require racket/format
         racket/list
         racket/math
         racket/sequence
         racket/stream
         racket/struct
         rebellion/binary/bit
         rebellion/binary/byte
         rebellion/type/tuple)

(module+ test
  (require (submod "..")
           rackunit
           rebellion/private/static-name))

;@------------------------------------------------------------------------------

;; TODO: put this in its own module
(define (immutable-bytes? v) (and (bytes? v) (immutable? v)))

;@------------------------------------------------------------------------------

(define (make-bitstring-properties descriptor)
  (define equal+hash (default-tuple-equal+hash descriptor))
  (define accessor (tuple-descriptor-accessor descriptor))
  (define custom-write
    (make-constructor-style-printer
     (λ (_) 'bitstring)
     (λ (this)
       (define bytes (accessor this 0))
       (define padding (accessor this 1))
       (define num-bytes (bytes-length bytes))
       (for/list
           ([byte (in-bytes bytes)]
            [n (in-naturals)]
            #:when #t
            [bit (in-range
                  (if (equal? n (sub1 num-bytes))
                      (- 8 padding)
                      8))])
         (byte-ref byte bit)))))
  (define (sequence-impl bits) (in-bitstring bits))
  (list (cons prop:custom-write custom-write)
        (cons prop:equal+hash equal+hash)
        (cons prop:sequence sequence-impl)))

(define-tuple-type bitstring (bytes padding)
  #:constructor-name plain-bitstring
  #:property-maker make-bitstring-properties)

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
  ;; bitstrings. However, half of that can be immediately reclaimed by GC so it
  ;; merely contributes to memory pressure.
  (define padded-bytes (bytes->immutable-bytes mutable-padded-bytes))

  (plain-bitstring padded-bytes padding-size))

(define (in-bitstring bits)
  (for/stream ([i (in-range (bitstring-size bits))])
    (bitstring-ref bits i)))

(define empty-bitstring (bitstring))

(define (bitstring->padded-bytes bits) (bitstring-bytes bits))

(define (bytes->bitstring bytes #:padding [padding 0])
  (cond [(zero? padding) (plain-bitstring bytes 0)]
        [else
         (define size (bytes-length bytes))
         (define last-pos (sub1 size))
         (define mutable-padded-bytes (make-bytes size 0))
         (bytes-copy! mutable-padded-bytes 0
                      bytes 0 last-pos)
         (define last-byte (bytes-ref bytes last-pos))
         (define padded-last-byte (byte-clear-rightmost-bits last-byte padding))
         (bytes-set! mutable-padded-bytes last-pos padded-last-byte)
         (define padded-bytes (bytes->immutable-bytes mutable-padded-bytes))
         (plain-bitstring padded-bytes padding)]))

(module+ test
  (test-case (name-string bitstring)
    (for ([i (in-range 0 100)])
      (define zero-bits (apply bitstring (make-list i 0)))
      (define one-bits (apply bitstring (make-list i 1)))
      (void))
    (test-case (name-string bitstring-bytes)
      (check-equal? (bitstring-bytes (bitstring 0 0 0 0 1 1 1 1
                                                1 1 0 0 1 1 0 0
                                                1 0 0 1))
                    (bytes (byte 0 0 0 0 1 1 1 1)
                           (byte 1 1 0 0 1 1 0 0)
                           (byte 1 0 0 1 0 0 0 0))))
    (test-case "bitstring-printed-representation"
      (check-equal? (~a (bitstring 0 1 1 0 0 1 0 1 1 1 1))
                    "#<bitstring: 0 1 1 0 0 1 0 1 1 1 1>")
      (check-equal? (~a (bitstring)) "#<bitstring:>"))))

(define (bitstring-size bits)
  (define num-bytes (bytes-length (bitstring->padded-bytes bits)))
  (- (* num-bytes 8) (bitstring-padding bits)))

(define (bitstring-ref bits pos)
  (define-values (byte-pos bit-pos) (quotient/remainder pos 8))
  (define bytes (bitstring->padded-bytes bits))
  (byte-ref (bytes-ref bytes byte-pos) bit-pos))

(module+ test
  (test-case (name-string bitstring-size)
    (check-equal? (bitstring-size (bitstring)) 0)
    (check-equal? (bitstring-size (bitstring 0)) 1)
    (check-equal? (bitstring-size (bitstring 1)) 1)
    (check-equal? (bitstring-size (bitstring 1 1 1 1 1 1 1 1)) 8)
    (check-equal? (bitstring-size (bitstring 1 1 1 1 1 1 1 1 1)) 9))
  (test-case (name-string bitstring-ref)
    (check-equal? (bitstring-ref (bitstring 0 1) 0) 0)
    (check-equal? (bitstring-ref (bitstring 0 1) 1) 1)
    (define bits
      (bitstring 0 0 0 0 0 1 0 0
                 0 0 0 1 1 1 1 0
                 1 1 1))
    (check-equal? (bitstring-ref bits 10) 0)
    (check-equal? (bitstring-ref bits 11) 1)
    (check-equal? (bitstring-ref bits 15) 0)
    (check-equal? (bitstring-ref bits 16) 1)))
