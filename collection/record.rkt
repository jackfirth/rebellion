#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [build-record (-> (-> keyword? any/c) keyset? record?)]
  [empty-record record?]
  [record (unconstrained-domain-> record?)]
  [record? predicate/c]
  [record-contains-key? (-> record? keyword? boolean?)]
  [record-map (-> record? (-> any/c any/c) record?)]
  [record-merge2
   (->* (record? record?) (#:merge (-> any/c any/c any/c)) record?)]
  [record-keywords (-> record? keyset?)]
  [record-values (-> record? immutable-vector?)]
  [record-ref (-> record? keyword? any/c)]
  [record-remove (-> record? keyword? record?)]
  [record-size (-> record? natural?)]
  [record-field (unconstrained-domain-> record-field?)]
  [record-field? predicate/c]
  [record-field-name (-> record-field? keyword?)]
  [record-field-value (-> record-field? any/c)]))

(require racket/list
         racket/math
         racket/set
         rebellion/collection/immutable-vector
         rebellion/collection/keyset
         rebellion/private/sequence-markup
         rebellion/type/tuple)

(module+ test
  (require (submod "..")
           racket/format
           rackunit))

;@------------------------------------------------------------------------------

(define (make-record-properties descriptor)
  (define accessor (tuple-descriptor-accessor descriptor))
  (define custom-write
    (make-constructor-style-printer
      'record
      (λ (this)
        (define keywords (accessor this 0))
        (define values (accessor this 1))
        (for/list ([kw (in-list (keyset->list keywords))]
                   [v (in-vector values)])
          (define kw-str (unquoted-printing-string (format "~s" kw)))
          (sequence-markup (list kw-str v))))))
  (list (cons prop:equal+hash (default-tuple-equal+hash descriptor))
        (cons prop:custom-write custom-write)))

(define-tuple-type record (keywords values)
  #:omit-root-binding
  #:property-maker make-record-properties)

(define (record-keyword-procedure kws vs)
  (constructor:record (list->keyset kws) (list->immutable-vector vs)))

(define record
  (procedure-reduce-keyword-arity
   (make-keyword-procedure record-keyword-procedure)
   0
   empty
   #f))

(define empty-record (record))

(define (record-size rec)
  (keyset-size (record-keywords rec)))

(define (record-ref rec kw)
  (define index (keyset-index-of (record-keywords rec) kw))
  (and index (immutable-vector-ref (record-values rec) index)))

(module+ test
  (define rec
    (record #:name "Alyssa P. Hacker"
            #:age 42
            #:favorite-color 'turqoise))
  (test-case "basic-integration-test"
    (check-equal? (record-size rec) 3)
    (check-equal? (record-keywords rec) (keyset #:age #:favorite-color #:name))
    (check-equal? (record-values rec)
                  (immutable-vector 42 'turqoise "Alyssa P. Hacker"))
    (define rec2
      (record #:name "Alyssa P. Hacker"
              #:age 42
              #:favorite-color 'turqoise))
    (check-equal? rec rec2)
    (check-equal? (equal-hash-code rec) (equal-hash-code rec2))
    (check-equal? (equal-secondary-hash-code rec)
                  (equal-secondary-hash-code rec2)))
  (test-case "record-ref"
    (check-equal? (record-ref rec '#:name) "Alyssa P. Hacker")
    (check-equal? (record-ref rec '#:foo) #f)))

(define (record-merge2 rec1 rec2
                       #:merge [merge-function (λ (a b) b)])
  (define keys1 (record-keywords rec1))
  (define keys2 (record-keywords rec2))
  (define values1 (record-values rec1))
  (define values2 (record-values rec2))
  (define merged-keys
    (set->keyset (set-union (keyset->set keys1) (keyset->set keys2))))
  (build-record (λ (kw)
                  (define in1? (keyset-contains? keys1 kw))
                  (define in2? (keyset-contains? keys2 kw))
                  (cond
                    [(and in1? in2?)
                     (merge-function (record-ref rec1 kw) (record-ref rec2 kw))]
                    [in1? (record-ref rec1 kw)]
                    [else (record-ref rec2 kw)]))
                merged-keys))

(define (sorted-keywords-and-values->record kws vs)
  (keyword-apply record kws vs empty))

(define (record-remove rec kw)
  (define keys (keyset-remove (record-keywords rec) kw))
  (build-record (λ (kw) (record-ref rec kw)) keys))

(define (record-map rec f)
  (define kws (record-keywords rec))
  (define vs (record-values rec))
  (define mapped-vs (immutable-vector-map f vs))
  (constructor:record kws mapped-vs))

(define (build-record builder keys)
  (define vs
    (vector->immutable-vector
     (for/vector #:length (keyset-size keys)
       ([kw (in-keyset keys)])
       (builder kw))))
  (constructor:record keys vs))

(define (record-contains-key? rec kw)
  (keyset-contains? (record-keywords rec) kw))

(module+ test
  (test-case "record-merge2"
    (check-equal? (record-merge2 empty-record empty-record) empty-record)
    (check-equal? (record-merge2 (record #:x 1 #:y 2) empty-record)
                  (record #:x 1 #:y 2))
    (check-equal? (record-merge2 empty-record (record #:x 1 #:y 2))
                  (record #:x 1 #:y 2))
    (check-equal? (record-merge2 (record #:a 1 #:b 2) (record #:x 1 #:y 2))
                  (record #:a 1 #:b 2 #:x 1 #:y 2))
    (check-equal? (record-merge2 (record #:foo 1) (record #:foo 2))
                  (record #:foo 2))
    (check-equal? (record-merge2 (record #:foo 1) (record #:foo 2)
                                 #:merge +)
                  (record #:foo 3)))
  (test-case "record-remove"
    (check-equal? (record-remove (record #:a 1 #:b 2 #:c 3) '#:b)
                  (record #:a 1 #:c 3)))
  (test-case "record-map"
    (check-equal? (record-map (record #:x 1 #:y -1 #:z 0) (λ (x) (* x 100)))
                  (record #:x 100 #:y -100 #:z 0)))
  (test-case "build-record"
    (check-equal? (build-record keyword->string (keyset #:x #:y #:z))
                  (record #:x "x" #:y "y" #:z "z")))
  (test-case "record-contains-key?"
    (check-true (record-contains-key? (record #:x 0 #:y 0) '#:x))
    (check-true (record-contains-key? (record #:x 0 #:y 0) '#:y))
    (check-false (record-contains-key? (record #:x 0 #:y 0) '#:z))))

;@------------------------------------------------------------------------------

(define (make-record-field-properties descriptor)
  (define type-name (tuple-type-name (tuple-descriptor-type descriptor)))
  (define accessor (tuple-descriptor-accessor descriptor))
  (define equal+hash (default-tuple-equal+hash descriptor))
  (define custom-write
    (make-constructor-style-printer
     type-name
     (λ (this)
       (define name (string-append "#:" (keyword->string (accessor this 0))))
       (list (unquoted-printing-string name) (accessor this 1)))))
  (list (cons prop:equal+hash equal+hash)
        (cons prop:custom-write custom-write)))

(define-tuple-type record-field (name value)
  #:omit-root-binding
  #:property-maker make-record-field-properties)

(define (record-field-keyword-function kws kw-args)
  (when (> (length kws) 1)
    (raise-arguments-error 'record-field
                           "multiple keyword arguments"
                           "keywords" kws
                           "values" kw-args))
  (when (< (length kws) 1)
    (raise-arguments-error 'record-field "no arguments given"))
  (constructor:record-field (first kws) (first kw-args)))

(define record-field
  (procedure-reduce-keyword-arity
   (make-keyword-procedure record-field-keyword-function)
   0
   empty
   #f))

(module+ test
  (test-case "record-field"
    (define f (record-field #:widget-price #e49.99))
    (check-equal? (record-field-name f) '#:widget-price)
    (check-equal? (record-field-value f) #e49.99)
    (check-equal? f (record-field #:widget-price #e49.99))
    (check-equal? (~v f) "(record-field #:widget-price 4999/100)")
    (check-equal? (~a f) "#<record-field: #:widget-price 4999/100>")
    (check-equal? (~s f) "#<record-field: #:widget-price 4999/100>")))
