#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [empty-record record?]
  [record (unconstrained-domain-> record?)]
  [record? (-> any/c boolean?)]
  [record-keywords (-> record? (listof keyword?))]
  [record-values (-> record? list?)]
  [record-ref (-> record? keyword? any/c)]
  [record-remove (-> record? keyword? record?)]
  [record-size (-> record? natural?)]))

(require racket/list
         racket/math
         racket/struct
         rebellion/generative-token)

(module+ test
  (require (submod "..")
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
