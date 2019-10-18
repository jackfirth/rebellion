#lang racket/base

(require racket/contract/base)

(provide
 permutation
 cyclic-permutation
 (contract-out
  [empty-permutation permutation?]
  [permutation? (-> any/c boolean?)]
  [permutation-ref
   (->i ([perm () permutation?]
         [pos (perm) (and/c natural? (</c (permutation-size perm)))])
        [_ (perm) (and/c natural? (</c (permutation-size perm)))])]
  [permutation-size (-> permutation? natural?)]
  [string-permute
   (->i ([str () (and/c string? immutable?)]
         [perm (str) (permutation-size/c (string-length str))])
        [_ () (and/c string? immutable?)])]
  [vector-permute
   (->i ([vec () (and/c vector? immutable?)]
         [perm (vec) (permutation-size/c (vector-length vec))])
        [_ () (and/c vector? immutable?)])]))

(require (for-syntax racket/base
                     racket/list)
         racket/math
         racket/struct
         rebellion/type/tuple
         syntax/parse/define)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define (make-permutation-properties descriptor)
  (define type-name (tuple-type-name (tuple-descriptor-type descriptor)))
  (define accessor (tuple-descriptor-accessor descriptor))
  (define custom-write
    (make-constructor-style-printer
     (λ (_) type-name)
     (λ (this) (accessor this 0))))
  (list (cons prop:equal+hash (default-tuple-equal+hash descriptor))
        (cons prop:custom-write custom-write)))

(define-tuple-type permutation (vector)
  #:constructor-name permutation-constructor
  #:property-maker make-permutation-properties)

(define (permutation-size/c expected-size)
  (flat-named-contract
   (list 'permutation-size/c expected-size)
   (λ (perm)
     (and (permutation? perm)
          (equal? (permutation-size perm) expected-size)))))

(define (permutation-size perm) (vector-length (permutation-vector perm)))
(define (permutation-ref perm pos) (vector-ref (permutation-vector perm) pos))

(define-for-syntax (check-duplicate-position positions)
  (check-duplicates (syntax->list positions) #:key syntax-e))

(define-for-syntax (check-position-bounds positions size)
  (findf (λ (pos) (>= (syntax-e pos) size)) (syntax->list positions)))

(define-simple-macro (permutation position:nat ...)
  #:do [(define size (length (syntax->list #'(position ...))))]
  #:fail-when (check-position-bounds #'(position ...) size)
  (format "position out of bounds for permutation of size ~a" size)
  #:fail-when (check-duplicate-position #'(position ...)) "duplicate position"
  (permutation-constructor (vector-immutable position ...)))

(define-simple-macro (cyclic-permutation position:nat ...+)
  #:do [(define positions
          (map syntax-e (syntax->list #'(position ...))))
        (define size (add1 (apply max 0 positions)))
        (define positions-vec
          (vector->immutable-vector (list->vector positions)))
        (define num-positions (vector-length positions-vec))]
  #:fail-when (check-duplicate-position #'(position ...)) "duplicate position"
  #:do [(define vec (build-vector size values))
        (for ([i (in-range 0 num-positions)])
          (define j (modulo (add1 i) num-positions))
          (define pos (vector-ref positions-vec i))
          (define next-pos (vector-ref positions-vec j))
          (vector-set! vec next-pos pos))]
  #:with vec #`(quote #,(vector->immutable-vector vec))
  (permutation-constructor vec))

(define empty-permutation (permutation))

(define (vector-permute vec perm)
  (vector->immutable-vector
   (build-vector (vector-length vec)
                 (λ (pos)
                   (vector-ref vec (permutation-ref perm pos))))))

(define (string-permute str perm)
  (string->immutable-string
   (build-string (string-length str)
                 (λ (pos)
                   (string-ref str (permutation-ref perm pos))))))

(module+ test
  (define perm (permutation 3 5 4 0 1 2))
  (test-case "permutation-size"
    (check-equal? (permutation-size perm) 6))
  (test-case "permutation-ref"
    (check-equal? (permutation-ref perm 0) 3)
    (check-equal? (permutation-ref perm 1) 5)
    (check-equal? (permutation-ref perm 2) 4)
    (check-equal? (permutation-ref perm 3) 0)
    (check-equal? (permutation-ref perm 4) 1)
    (check-equal? (permutation-ref perm 5) 2))
  (test-case "vector-permute"
    (define vec (vector-immutable 'a 'b 'c 'd 'e 'f))
    (check-equal? (vector-permute vec perm)
                  (vector-immutable 'd 'f 'e 'a 'b 'c)))
  (test-case "string-permute"
    (check-pred immutable? (string-permute "abcdef" perm))
    (check-equal? (string-permute "abcdef" perm) "dfeabc"))
  (test-case "empty-permutation"
    (check-equal? (permutation-size empty-permutation) 0)
    (check-equal? (string-permute "" empty-permutation) ""))
  (test-case "cyclic-permutation"
    (define perm (cyclic-permutation 0 2 4 6 8))
    (check-equal? (permutation-size perm) 9)
    (check-equal? (string-permute "abcdefhij" perm) "jbadcfeih")))
