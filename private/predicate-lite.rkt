#lang rebellion/private/dependencies/layer0

(provide
 (contract-out
  [predicate? predicate?]
  [predicate-name (-> predicate? name?)]
  [predicate-rename (-> predicate? name? predicate?)]
  [predicate-domain (-> predicate? contract?)]
  [predicate/c (-> flat-contract? contract?)]
  [predicate-apply (-> predicate? any/c boolean?)]
  [make-predicate (->* ((-> any/c boolean?)) (#:name name?) predicate?)]))

(require rebellion/private/name-lite)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------
;; structure type

(struct predicate-impl (name proc)
  #:reflection-name 'predicate
  #:constructor-name plain-make-predicate
  #:property prop:name (λ (this) (predicate-name this))
  #:property prop:type-name 'predicate
  #:methods gen:custom-write [(define write-proc write-named-value)]

  #:property prop:flat-contract
  (build-flat-contract-property
   #:name (λ (this) (predicate-name-for-flat-contract this))
   #:first-order (λ (this) (λ (v) (predicate-apply this v))))

  #:methods gen:equal+hash
  [(define type-tag (gensym 'predicate-type-tag))
   (define type-tag2 (gensym 'predicate-type-tag))
   (define (equal-proc this other recur)
     (and (recur (predicate-name this) (predicate-name other))
          (recur (predicate-proc this) (predicate-proc other))))
   (define (hash-proc this recur)
     (recur (list type-tag (predicate-name this) (predicate-proc this))))
   (define (hash2-proc this recur)
     (recur (list type-tag2 (predicate-name this) (predicate-proc this))))])

(define predicate-name predicate-impl-name)
(define predicate-proc predicate-impl-proc)

(define (make-predicate proc #:name [name unknown-name])
  (plain-make-predicate name proc))

(define predicate?
  (make-predicate predicate-impl? #:name (symbolic-name 'predicate?)))

(define (predicate-rename pred name)
  (make-predicate (predicate-proc pred) #:name name))

(define (predicate-apply pred v) ((predicate-proc pred) v))

(define (predicate-name-for-flat-contract pred)
  (name->s-expression (predicate-name pred)
                      #:unknown 'anonymous-predicate))

(define (predicate-chaperone pred guard-proc . props+vs)
  (define pred-proc (predicate-proc pred))
  (define pred-name (predicate-name pred))
  (define pred-proc/guard (chaperone-procedure pred-proc guard-proc))
  (define pred/guard (make-predicate pred-proc/guard #:name pred-name))
  (apply chaperone-struct pred/guard struct:predicate-impl props+vs))

;@------------------------------------------------------------------------------
;; contracts

(struct predicate-contract
  (domain
   name
   first-order
   late-neg-projection
   val-first-projection
   projection)

  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name (λ (this) (predicate-contract-name this))
   #:first-order (λ (this) (predicate-contract-first-order this))
   #:late-neg-projection
   (λ (this) (predicate-contract-late-neg-projection this))
   #:val-first-projection
   (λ (this blm) ((predicate-contract-val-first-projection this) blm))
   #:projection (λ (this) (predicate-contract-projection this)))

  #:methods gen:custom-write
  [(define write-proc contract-custom-write-property-proc)]

  #:methods gen:equal+hash
  [(define type-tag (gensym 'predicate-contract))
   (define type-tag2 (gensym 'predicate-contract))
   (define (equal-proc this other recur)
     (recur (predicate-contract-domain this) (predicate-contract-domain other)))
   (define (hash-proc p recur)
     (recur (list type-tag (predicate-name p) (predicate-proc p))))
   (define (hash2-proc p recur)
     (recur (list type-tag2 (predicate-name p) (predicate-proc p))))])

(define/final-prop (predicate/c uncoerced-domain)
  (define domain (coerce-flat-contract 'predicate/c uncoerced-domain))
  (define name (build-compound-type-name 'predicate/c domain))
  (define (first-order pred) (predicate? pred))
  (define domain-projection (contract-late-neg-projection domain))
  (define (late-neg-projection blm)
    (define blm/domain-context
      (blame-add-context blm "the predicate domain of" #:swap? #t))
    (define domain-projection/blame (domain-projection blm/domain-context))
    (λ (pred missing-context)
      (define (check-domain v) (domain-projection/blame v missing-context))
      (predicate-chaperone pred check-domain
                           impersonator-prop:contracted the-predicate-contract
                           impersonator-prop:blame (cons blm missing-context))))
  (define (val-first-projection blm)
    (define late-neg/blame (late-neg-projection blm))
    (λ (v) (λ (missing-party) (late-neg/blame v missing-party))))
  (define (projection blm)
    (define late-neg/blame (late-neg-projection blm))
    (λ (v) (late-neg/blame v #f)))
  (define the-predicate-contract
    (predicate-contract domain
                        name
                        first-order
                        late-neg-projection
                        val-first-projection
                        projection))
  the-predicate-contract)

(define (predicate-domain pred)
  (if (has-contract? pred)
      (predicate-contract-domain (value-contract pred))
      any/c))

(module+ test
  (define/contract test-pred
    (predicate/c boolean?)
    (make-predicate (λ (_) #t) #:name (symbolic-name 'test-pred)))
  (test-case "predicate-value-contract"
    (check-equal? (value-contract test-pred) (predicate/c boolean?)))
  (test-case "predicate-domain"
    (check-equal? (predicate-domain test-pred)
                  (coerce-flat-contract 'predicate-domain-test boolean?)))
  (test-case "predicate-apply"
    (check-true (predicate-apply test-pred #f))
    (check-exn exn:fail:contract?
               (λ () (predicate-apply test-pred "not-a-boolean"))))
  (test-case "predicate-callable-as-function"
    (check-true (test-pred #t))
    (check-exn exn:fail:contract? (λ () (test-pred "not-a-boolean")))))
