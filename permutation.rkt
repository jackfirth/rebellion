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
  [permute (->* ((sequence/c any/c) permutation?) (#:into reducer?) any/c)]
  [permuting (-> permutation? transducer?)]
  [permutation-reverse (-> permutation? permutation?)]))


(require (for-syntax racket/base
                     racket/list)
         racket/match
         racket/math
         racket/sequence
         racket/struct
         rebellion/base/variant
         rebellion/collection/list
         rebellion/collection/vector
         rebellion/private/static-name
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/tuple
         syntax/parse/define)


(module+ test
  (require (submod "..")
           rackunit
           rebellion/collection/list))


;@----------------------------------------------------------------------------------------------------


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
  #:omit-root-binding
  #:property-maker make-permutation-properties)


(define (permutation-size/c expected-size)
  (flat-named-contract
   (list 'permutation-size/c expected-size)
   (λ (perm)
     (and (permutation? perm)
          (equal? (permutation-size perm) expected-size)))))


(define (permutation-size perm)
  (vector-length (permutation-vector perm)))


(define (permutation-ref perm pos)
  (vector-ref (permutation-vector perm) pos))


(define-for-syntax (check-duplicate-position positions)
  (check-duplicates (syntax->list positions) #:key syntax-e))


(define-for-syntax (check-position-bounds positions size)
  (findf (λ (pos) (>= (syntax-e pos) size)) (syntax->list positions)))


(define-simple-macro (permutation position:nat ...)
  #:do [(define size (length (syntax->list #'(position ...))))]
  #:fail-when (check-position-bounds #'(position ...) size)
  (format "position out of bounds for permutation of size ~a" size)
  #:fail-when (check-duplicate-position #'(position ...)) "duplicate position"
  (constructor:permutation (vector-immutable position ...)))


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
  (constructor:permutation vec))


(define (permutation-reverse perm)
  (define size (permutation-size perm))
  (define original (permutation-vector perm))
  (define reversed (make-vector size))
  (for ([i (in-vector original)])
    (vector-set! reversed (vector-ref original i) i))
  (constructor:permutation (vector->immutable-vector reversed)))


(define empty-permutation (permutation))


(struct permuting-state (consumption-count emission-count) #:transparent)


(define no-value (gensym "no-value"))


(define (permuting perm)
  (let ([perm (permutation-reverse perm)])
    (define size (permutation-size perm))
    (define temp-storage (make-vector size no-value))

    (make-transducer

     #:starter
     (if (zero? size)
         (λ () (variant #:finish #false))
         (λ () (variant #:consume (permuting-state 0 0))))

     #:consumer
     (λ (state element)
       (match-define (permuting-state consumption-count emission-count) state)
       (define next-consumption-count (add1 consumption-count))

       (when (equal? consumption-count size)
         ;; If we've already consumed size elements, then the input sequence was too long. We fail
         ;; hard in that case to avoid silently discarding elements.
         (raise-arguments-error
          (name permuting)
          "expected fewer elements"
          "elements received" next-consumption-count
          "elements expected" size))

       (define next-emission (permutation-ref perm consumption-count))
       (vector-set! temp-storage next-emission element)
       (define next-state (permuting-state next-consumption-count emission-count))
       (cond
         ;; If this is the last element needed, try consuming one more element so we can check that
         ;; the sequence wasn't too long.
         [(equal? next-consumption-count size) (variant #:consume next-state)]

         [(equal? next-emission emission-count) (variant #:emit next-state)]
         [else (variant #:consume next-state)]))

     #:emitter
     (λ (state)
       (match-define (permuting-state consumption-count emission-count) state)
       (define element (vector-ref temp-storage emission-count))
       (vector-set! temp-storage emission-count no-value)
       (define next-emission-count (add1 emission-count))
       (define next-state (permuting-state consumption-count next-emission-count))
       (define tagged-next-state
         (if (equal? (vector-ref temp-storage next-emission-count) no-value)
             (variant #:consume next-state)
             (variant #:emit next-state)))
       (emission tagged-next-state element))

     #:half-closer
     (λ (state)
       (match-define (permuting-state consumption-count emission-count) state)
       (unless (equal? consumption-count size)
         (raise-arguments-error
          (name permuting)
          "expected more elements"
          "elements received" consumption-count
          "elements expected" size))
       (variant #:half-closed-emit state))

     #:half-closed-emitter
     (λ (state)
       (match-define (permuting-state consumption-count emission-count) state)
       (define element (vector-ref temp-storage emission-count))
       (vector-set! temp-storage emission-count no-value)
       (define next-emission-count (add1 emission-count))
       (define tagged-next-state
         (if (equal? next-emission-count size)
             (variant #:finish #false)
             (variant #:half-closed-emit (permuting-state consumption-count next-emission-count))))
       (half-closed-emission tagged-next-state element))

     #:finisher void

     #:name (name permuting))))


(define (permute seq perm #:into [reducer #false])
  (cond

    ;; Fast paths for common sequence types
    [(and (vector? seq) (not reducer)) (vector-permute seq perm)]
    [(and (list? seq) (not reducer)) (vector-permute (list->vector seq) perm)]
    [(and (list? seq) (equal? reducer into-list)) (list-permute seq perm)]
    [(and (string? seq) (equal? reducer into-string)) (string-permute seq perm)]

    ;; Slow generic path
    [else
     (let ([reducer (or reducer (into-vector #:size (permutation-size perm)))])
       (transduce seq (permuting perm) #:into reducer))]))


(define (check-permutation-size-constraints elements-received perm)
  (define elements-expected (permutation-size perm))

  (when (< elements-received elements-expected)
    (raise-arguments-error

     ;; check-permutation-size-constraints is only used by permute or functions that are internal
     ;; implementation details of permute, like list-permute and vector-permute.
     (name permute)

     "expected more elements"
     "elements received" elements-received
     "elements expected" elements-expected))

  (when (> elements-received elements-expected)
    (raise-arguments-error
     (name permute) ;; see above comment
     "expected fewer elements"
     "elements received" elements-received
     "elements expected" elements-expected)))


(define (vector-permute vec perm)
  (check-permutation-size-constraints (vector-length vec) perm)
  (vector->immutable-vector
   (build-vector (vector-length vec) (λ (pos) (vector-ref vec (permutation-ref perm pos))))))


(define (string-permute str perm)
  (check-permutation-size-constraints (string-length str) perm)
  (string->immutable-string
   (build-string (string-length str) (λ (pos) (string-ref str (permutation-ref perm pos))))))


(define (list-permute xs perm)
  (define vec (list->vector xs))
  (check-permutation-size-constraints (vector-length vec) perm)
  (build-list (vector-length vec) (λ (pos) (vector-ref vec (permutation-ref perm pos)))))


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

  (test-case "permute"
    (check-equal? (permute '(a b c d e f) perm) (vector-immutable 'd 'f 'e 'a 'b 'c))
    (check-equal?
     (permute (vector-immutable 'a 'b 'c 'd 'e 'f) perm)
     (vector-immutable 'd 'f 'e 'a 'b 'c))
    (check-equal? (permute '(a b c d e f) perm #:into into-list) '(d f e a b c))
    (check-equal? (permute "abcdef" perm #:into into-string) "dfeabc")
    (check-equal? (permute "abcdef" perm #:into into-list) '(#\d #\f #\e #\a #\b #\c))

    (test-case "too many elements"
      (check-exn exn:fail:contract? (λ () (permute "abcdefG" perm)))
      (check-exn #rx"expected fewer elements" (λ () (permute "abcdefG" perm)))
      (check-exn #rx"elements received: 7" (λ () (permute "abcdefG" perm)))
      (check-exn #rx"elements expected: 6" (λ () (permute "abcdefG" perm))))

    (test-case "too few elements"
      (check-exn exn:fail:contract? (λ () (permute perm "abcde")))
      (check-exn #rx"expected more elements" (λ () (permute "abcde" perm)))
      (check-exn #rx"elements received: 5" (λ () (permute "abcde" perm)))
      (check-exn #rx"elements expected: 6" (λ () (permute "abcde" perm)))))

  (test-case "empty-permutation"
    (check-equal? (permutation-size empty-permutation) 0)
    (check-equal? (permute "" empty-permutation #:into into-string) ""))

  (test-case "cyclic-permutation"
    (define perm (cyclic-permutation 0 2 4 6 8))
    (check-equal? (permutation-size perm) 9)
    (check-equal? (permute "abcdefhij" perm #:into into-string) "jbadcfeih"))

  (test-case "permutation-reverse"
    (check-equal? (permutation-reverse (permutation 1 2 3 4 0)) (permutation 4 0 1 2 3))))
