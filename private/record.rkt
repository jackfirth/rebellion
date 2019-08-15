#lang parendown racket/base

(require racket/contract/base)

(provide #/contract-out
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
  [record-ref-maybe (-> record? keyword? maybe?)]
  [record-ref (-> record? keyword? any/c)]
  [record-remove (-> record? keyword? record?)]
  [record-size (-> record? natural?)]
  [record-field (unconstrained-domain-> record-field?)]
  [record-field? predicate/c]
  [record-field-name (-> record-field? keyword?)]
  [record-field-value (-> record-field? any/c)])

(require racket/list
         racket/math
         racket/set
         racket/struct
         (only-in lathe-comforts dissectfn dissect expect fn mat w-)
         (only-in lathe-comforts/maybe just maybe? maybe-if)
         rebellion/base/generative-token
         rebellion/collection/immutable-vector
         rebellion/collection/keyset
         rebellion/private/spliced-printing-entry
         rebellion/type/record
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
      (fn this 'record)
      (fn this
        (w- keywords (accessor this 0)
        #/w- values (accessor this 1)
        #/for/list ([kw (in-list (keyset->list keywords))]
                   [v (in-vector values)])
          (define kw-str (unquoted-printing-string (format "~s" kw)))
          (spliced-printing-entry kw-str v)))))
  (list (cons prop:equal+hash (make-tuple-equal+hash descriptor))
        (cons prop:custom-write custom-write)))

(define-tuple-type record (keywords values)
  #:constructor-name internal-record-constructor
  #:property-maker make-record-properties)

(define (record-keyword-procedure kws vs)
  (internal-record-constructor (list->keyset kws) (list->immutable-vector vs)))

(define record
  (procedure-reduce-keyword-arity
   (make-keyword-procedure record-keyword-procedure)
   0
   empty
   #f))

(define empty-record (record))

(define (dissect-record rec body)
  (body (record-keywords rec) (record-values rec)))

(define (record-size rec)
  (dissect-record rec #/fn kws vs
  #/keyset-size kws))

(define (record-ref-maybe rec kw)
  (dissect-record rec #/fn kws vs
  #/w- index (keyset-index-of kws kw)
  #/maybe-if index #/fn #/immutable-vector-ref vs index))

(define (record-ref rec kw)
  (mat (record-ref-maybe rec kw) (just result) result
    #f))

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
                       #:merge [merge-function (fn a b b)])
  (dissect-record rec1 #/fn keys1 values1
  #/dissect-record rec2 #/fn keys2 values2
  #/w- merged-keys
    (set->keyset #/set-union (keyset->set keys1) (keyset->set keys2))
  #/build-record (fn kw
                   (expect (keyset-contains? keys1 kw) #t (record-ref rec2 kw)
                   #/expect (keyset-contains? keys2 kw) #t (record-ref rec1 kw)
                   #/merge-function (record-ref rec1 kw) (record-ref rec2 kw)))
                 merged-keys))

(define (sorted-keywords-and-values->record kws vs)
  (keyword-apply record kws vs empty))

(define (record-remove rec kw)
  (define keys (keyset-remove (record-keywords rec) kw))
  (build-record (fn kw #/record-ref rec kw) keys))

(define (record-map rec f)
  (dissect-record rec #/fn kws vs
  #/w- mapped-vs (immutable-vector-map f vs)
  #/internal-record-constructor kws mapped-vs))

(define (build-record builder keys)
  (define vs
    (vector->immutable-vector
     (for/vector #:length (keyset-size keys)
       ([kw (in-keyset keys)])
       (builder kw))))
  (internal-record-constructor keys vs))

(define (record-contains-key? rec kw)
  (dissect-record rec #/fn kws vs
  #/keyset-contains? kws kw))

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
    (check-equal? (record-map (record #:x 1 #:y -1 #:z 0) #/fn x #/* x 100)
                  (record #:x 100 #:y -100 #:z 0)))
  (test-case "build-record"
    (check-equal? (build-record keyword->string (keyset #:x #:y #:z))
                  (record #:x "x" #:y "y" #:z "z")))
  (test-case "record-contains-key?"
    (check-true #/record-contains-key? (record #:x 0 #:y 0) '#:x)
    (check-true #/record-contains-key? (record #:x 0 #:y 0) '#:y)
    (check-false #/record-contains-key? (record #:x 0 #:y 0) '#:z)))

;@------------------------------------------------------------------------------

(define (make-record-field-properties descriptor)
  (define type-name (tuple-type-name #/tuple-descriptor-type descriptor))
  (define accessor (tuple-descriptor-accessor descriptor))
  (define equal+hash (make-tuple-equal+hash descriptor))
  (define custom-write
    (make-constructor-style-printer
     (dissectfn _ type-name)
     (fn this
       (w- name (string-append "#:" (keyword->string #/accessor this 0))
       #/list (unquoted-printing-string name) (accessor this 1)))))
  (list (cons prop:equal+hash equal+hash)
        (cons prop:custom-write custom-write)))

(define-tuple-type record-field (name value)
  #:constructor-name constructor:record-field
  #:property-maker make-record-field-properties)

(define (record-field-keyword-function kws kw-args)
  (expect kws (cons kw kws-rest)
    (raise-arguments-error 'record-field "no arguments given")
  #/expect kws-rest (list)
    (raise-arguments-error 'record-field
                           "multiple keyword arguments"
                           "keywords" kws
                           "values" kw-args)
  #/dissect kw-args (list kw-arg)
  #/constructor:record-field kw kw-arg))

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
