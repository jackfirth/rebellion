#lang racket/base

(require racket/contract/base)

(provide
 keyset
 (contract-out
  [empty-keyset keyset?]
  [keyset? predicate?]
  [keyset-contains? (-> keyset? keyword? boolean?)]
  [keyset-index-of
   (->i ([keys keyset?] [kw keyword?])
        [_ (keys) (or/c (and/c natural? (</c (keyset-size keys))) #f)])]
  [keyset-ref
   (->i ([keys keyset?]
         [pos (keys) (and/c natural? (</c (keyset-size keys)))])
        [_ keyword?])]
  [keyset-size (-> keyset? natural?)]
  [keyset->list (-> keyset? (listof keyword?))]
  [list->keyset (-> (listof keyword?) keyset?)]))

(require (for-syntax racket/base
                     racket/list)
         racket/list
         rebellion/collection/immutable-vector
         rebellion/base/generative-token
         rebellion/name
         (except-in rebellion/predicate predicate/c)
         rebellion/private/boolean
         rebellion/private/keyword
         rebellion/private/natural
         syntax/parse/define)

(module+ test
  (require (submod "..")
           racket/format
           rackunit))

;@------------------------------------------------------------------------------

(define (write-keyset keys out mode)
  (write-string "(keyset" out)
  (define size (keyset-size keys))
  (let loop ([i 0])
    (when (< i size)
      (write-string " #:" out)
      (write-string (keyword->string (keyset-ref keys i)) out)
      (loop (add1 i))))
  (write-string ")" out))

(define keyset-datatype-token (make-generative-token))

(struct keyset-impl (sorted-vector index-hash)
  #:reflection-name 'keyset
  #:constructor-name plain-make-keyset
  #:omit-define-syntaxes
  
  #:methods gen:equal+hash
  [(define (equal-proc this other recur)
     (recur (keyset-sorted-vector this) (keyset-sorted-vector other)))
   (define (hash-proc this recur)
     (recur (list keyset-datatype-token (keyset-sorted-vector this))))
   (define hash2-proc hash-proc)]
  
  #:methods gen:custom-write [(define write-proc write-keyset)])

(define keyset? (make-predicate keyset-impl? #:name (symbolic-name 'keyset?)))
(define keyset-sorted-vector keyset-impl-sorted-vector)
(define keyset-index-hash keyset-impl-index-hash)

(define (keyset-size keys)
  (vector-length (keyset-sorted-vector keys)))

(define (keyset-ref keys pos)
  (vector-ref (keyset-sorted-vector keys) pos))

(define (keyset-index-of keys kw)
  (hash-ref (keyset-index-hash keys) kw #f))

(define (keyset-contains? keys kw)
  (hash-has-key? (keyset-index-hash keys) kw))

(define (make-keyset sorted-keywords-vec)
  (define size (vector-length sorted-keywords-vec))
  (define index-hash
    (let loop ([i 0] [h (hasheq)])
      (if (< i size)
          (loop (add1 i)
                (hash-set h (vector-ref sorted-keywords-vec i) i))
          h)))
  (plain-make-keyset sorted-keywords-vec index-hash))

(define-simple-macro (keyset kw:keyword ...)
  #:do [(define kws (map syntax-e (syntax->list #'(kw ...))))
        (define sorted-kws (remove-duplicates (sort kws keyword<?)))
        (define sorted-kw-vec
          (vector->immutable-vector (list->vector sorted-kws)))
        (define sorted-kw-vec-stx (datum->syntax this-syntax sorted-kw-vec))]
  #:with sorted-kw-vec-literal sorted-kw-vec-stx
  (make-keyset 'sorted-kw-vec-literal))

(define empty-keyset (keyset))

(module+ test
  (test-case "empty-keyset"
    (check-equal? empty-keyset (keyset))
    (check-equal? (keyset-size empty-keyset) 0))
  (test-case "keyset"
    (define keys (keyset #:apple #:orange #:banana))
    (check-pred keyset? keys)
    (check-equal? (keyset-size keys) 3)
    (check-true (keyset-contains? keys '#:apple))
    (check-true (keyset-contains? keys '#:orange))
    (check-true (keyset-contains? keys '#:banana))
    (check-false (keyset-contains? keys '#:chocolate))
    (check-equal? (keyset-ref keys 0) '#:apple)
    (check-equal? (keyset-ref keys 1) '#:banana)
    (check-equal? (keyset-ref keys 2) '#:orange)
    (check-equal? (keyset-index-of keys '#:apple) 0)
    (check-equal? (keyset-index-of keys '#:orange) 2)
    (check-equal? (keyset-index-of keys '#:banana) 1)
    (check-false (keyset-index-of keys '#:chocolate))
    (check-equal? (~v keys) "(keyset #:apple #:banana #:orange)")
    (check-equal? (~a keys) "(keyset #:apple #:banana #:orange)")
    (check-equal? (~s keys) "(keyset #:apple #:banana #:orange)")))

(define (keyset->list keys)
  (build-list (keyset-size keys) (λ (i) (keyset-ref keys i))))

(define (list->keyset kws)
  (make-keyset
   (list->immutable-vector (remove-duplicates (sort kws keyword<?)))))

(module+ test
  (test-case "keyset->list"
    (check-equal? (keyset->list empty-keyset) (list))
    (check-equal? (keyset->list (keyset #:apple #:orange #:banana))
                  (list '#:apple '#:banana '#:orange)))
  (test-case "list->keyset"
    (check-equal? (list->keyset (list)) empty-keyset)
    (check-equal? (list->keyset (list '#:apple '#:orange '#:banana))
                  (keyset #:apple #:orange #:banana))
    (check-equal? (list->keyset (list '#:apple '#:apple '#:orange))
                  (keyset #:apple #:orange))))
