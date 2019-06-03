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
  [record-keywords (-> record? (listof keyword?))]
  [record-values (-> record? list?)]
  [record-ref (-> record? keyword? any/c)]
  [record-remove (-> record? keyword? record?)]
  [record-size (-> record? natural?)]
  [record-field (unconstrained-domain-> record-field?)]
  [record-field? predicate/c]
  [record-field-name (-> record-field? keyword?)]
  [record-field-value (-> record-field? any/c)]))

(require racket/list
         racket/math
         racket/struct
         rebellion/collection/keyset
         rebellion/generative-token
         rebellion/equal+hash/tuple
         rebellion/tuple-type
         rebellion/tuple-type-definition)

(module+ test
  (require (submod "..")
           racket/format
           rackunit))

;@------------------------------------------------------------------------------

(define record-datatype-token (make-generative-token))

(struct record (keywords values)
  #:constructor-name plain-record
  #:omit-define-syntaxes
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (this) 'record)
      (λ (this)
        (apply append
               (for/list ([kw (in-list (record-keywords this))]
                          [v (in-list (record-values this))])
                 (list (unquoted-printing-string (format "~s" kw)) v))))))]
  #:methods gen:equal+hash
  [(define (equal-proc this other recur)
     (and (recur (record-keywords this) (record-keywords other))
          (recur (record-values this) (record-values other))))
   (define (hash-proc this recur)
     (recur (list record-datatype-token
                  (record-keywords this)
                  (record-values this))))
   (define hash2-proc hash-proc)])

(define record
  (procedure-reduce-keyword-arity (make-keyword-procedure plain-record)
                                  0
                                  empty
                                  #f))

(define empty-record (record))

(define (record-size rec)
  (length (record-keywords rec)))

(module+ test
  (define rec
    (record #:name "Alyssa P. Hacker"
            #:age 42
            #:favorite-color 'turqoise))
  (check-equal? (record-size rec) 3)
  (check-equal? (record-keywords rec) (list '#:age '#:favorite-color '#:name))
  (check-equal? (record-values rec) (list 42 'turqoise "Alyssa P. Hacker"))
  (define rec2
    (record #:name "Alyssa P. Hacker"
            #:age 42
            #:favorite-color 'turqoise))
  (check-equal? rec rec2)
  (check-equal? (equal-hash-code rec) (equal-hash-code rec2))
  (check-equal? (equal-secondary-hash-code rec)
                (equal-secondary-hash-code rec2)))

(define (record-ref rec kw)
  (let loop ([kws (record-keywords rec)]
             [vs (record-values rec)])
    (cond [(empty? kws) #f]
          [(equal? (first kws) kw) (first vs)]
          [else (loop (rest kws) (rest vs))])))

(module+ test
  (test-case "record-ref"
    (define rec
      (record #:name "Alyssa P. Hacker"
              #:age 42
              #:favorite-color 'turqoise))
    (check-equal? (record-ref rec '#:name) "Alyssa P. Hacker")
    (check-equal? (record-ref rec '#:foo) #f)))

(define (record-merge2 rec1 rec2
                       #:merge [merge-function (λ (a b) b)])
  (let loop ([rec1-kws (record-keywords rec1)]
             [rec2-kws (record-keywords rec2)]
             [rec1-vs (record-values rec1)]
             [rec2-vs (record-values rec2)]
             [merged-kws empty]
             [merged-vs empty])
    (define rec1-empty? (empty? rec1-kws))
    (define rec2-empty? (empty? rec2-kws))
    (cond
      [(and rec1-empty? rec2-empty?)
       (keyword-apply record
                      (reverse merged-kws)
                      (reverse merged-vs)
                      empty)]
      [rec1-empty?
       (loop empty
             (rest rec2-kws)
             empty
             (rest rec2-vs)
             (cons (first rec2-kws) merged-kws)
             (cons (first rec2-vs) merged-vs))]
      [rec2-empty?
       (loop (rest rec1-kws)
             empty
             (rest rec1-vs)
             empty
             (cons (first rec1-kws) merged-kws)
             (cons (first rec1-vs) merged-vs))]
      [else
       (define kw1 (first rec1-kws))
       (define kw2 (first rec2-kws))
       (define v1 (first rec1-vs))
       (define v2 (first rec2-vs))
       (cond
         [(equal? kw1 kw2)
          (loop (rest rec1-kws)
                (rest rec2-kws)
                (rest rec1-vs)
                (rest rec2-vs)
                (cons kw1 merged-kws)
                (cons (merge-function v1 v2) merged-vs))]
         [(keyword<? kw1 kw2)
          (loop (rest rec1-kws)
                rec2-kws
                (rest rec1-vs)
                rec2-vs
                (cons kw1 merged-kws)
                (cons v1 merged-vs))]
         [else
          (loop rec1-kws
                (rest rec2-kws)
                rec1-vs
                (rest rec2-vs)
                (cons kw2 merged-kws)
                (cons v2 merged-vs))])])))

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
                  (record #:foo 3))))

(define (sorted-keywords-and-values->record kws vs)
  (keyword-apply record kws vs empty))

(define (record-remove rec kw)
  (let loop ([kws (record-keywords rec)]
             [vs (record-values rec)]
             [checked-kws empty]
             [checked-vs empty])
    (cond
      [(empty? kws)
       (sorted-keywords-and-values->record (reverse checked-kws)
                                           (reverse checked-vs))]
      [(equal? (first kws) kw)
       (sorted-keywords-and-values->record
        (append (reverse checked-kws) (rest kws))
        (append (reverse checked-vs) (rest vs)))]
      [else
       (loop (rest kws)
             (rest vs)
             (cons (first kws) checked-kws)
             (cons (first vs) checked-vs))])))

(module+ test
  (test-case "record-remove"
    (check-equal? (record-remove (record #:a 1 #:b 2 #:c 3) '#:b)
                  (record #:a 1 #:c 3))))

(define (record-map rec f)
  (define kws (record-keywords rec))
  (define vs (record-values rec))
  (define mapped-vs (map f vs))
  (sorted-keywords-and-values->record kws mapped-vs))

(module+ test
  (test-case "record-map"
    (check-equal? (record-map (record #:x 1 #:y -1 #:z 0) (λ (x) (* x 100)))
                  (record #:x 100 #:y -100 #:z 0))))

(define (build-record builder keys)
  (define size (keyset-size keys))
  (define vs (build-list size (λ (i) (builder (keyset-ref keys i)))))
  (keyword-apply record (keyset->list keys) vs (list)))

(module+ test
  (test-case "build-record"
    (check-equal? (build-record keyword->string (keyset #:x #:y #:z))
                  (record #:x "x" #:y "y" #:z "z"))))

(define (record-contains-key? rec kw)
  (and (member kw (record-keywords rec)) #t))

(module+ test
  (test-case "record-contains-key?"
    (check-true (record-contains-key? (record #:x 0 #:y 0) '#:x))
    (check-true (record-contains-key? (record #:x 0 #:y 0) '#:y))
    (check-false (record-contains-key? (record #:x 0 #:y 0) '#:z))))

;@------------------------------------------------------------------------------

(define (make-record-field-properties descriptor)
  (define type-name (tuple-type-name (tuple-descriptor-accessor descriptor)))
  (define accessor (tuple-descriptor-accessor descriptor))
  (define equal+hash (make-tuple-equal+hash descriptor))
  (define custom-write
    (make-constructor-style-printer
     (λ (_) type-name)
     (λ (this)
       (define name (string-append "#:" (keyword->string (accessor this 0))))
       (list (unquoted-printing-string name) (accessor this 1)))))
  (list (cons prop:equal+hash equal+hash)
        (cons prop:custom-write custom-write)))

(define-tuple-type record-field (name value)
  #:constructor constructor:record-field
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
