#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [atomic-fixnum? predicate/c]
  [make-atomic-fixnum
   (->* (fixnum?) (#:name (or/c interned-symbol? #false)) atomic-fixnum?)]
  [atomic-fixnum-get (-> atomic-fixnum? fixnum?)]
  [rename set-atomic-fixnum-get! atomic-fixnum-set!
          (-> atomic-fixnum? fixnum? void?)]
  [atomic-fixnum-add! (-> atomic-fixnum? fixnum? void?)]
  [atomic-fixnum-update! (-> atomic-fixnum? (-> fixnum? fixnum?) void?)]
  [atomic-fixnum-compare-and-set! (-> atomic-fixnum? fixnum? fixnum? boolean?)]
  [atomic-fixnum-compare-and-add! (-> atomic-fixnum? fixnum? fixnum? boolean?)]
  [atomic-fixnum-compare-and-exchange!
   (-> atomic-fixnum? fixnum? fixnum? fixnum?)]
  [atomic-fixnum-get-then-set! (-> atomic-fixnum? fixnum? fixnum?)]
  [atomic-fixnum-get-then-add! (-> atomic-fixnum? fixnum? fixnum?)]
  [atomic-fixnum-add-then-get! (-> atomic-fixnum? fixnum? fixnum?)]
  [atomic-fixnum-get-then-update!
   (-> atomic-fixnum? (-> fixnum? fixnum?) fixnum?)]
  [atomic-fixnum-update-then-get!
   (-> atomic-fixnum? (-> fixnum? fixnum?) fixnum?)]))

(require (only-in racket/unsafe/ops unsafe-struct*-cas!)
         racket/fixnum
         rebellion/base/symbol
         rebellion/private/static-name
         rebellion/private/guarded-block
         syntax/parse/define)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define-logger rebellion/concurrency/atomic/fixnum)

(define-simple-macro (log-atomic-fixnum-contention num:id)
  (log-rebellion/concurrency/atomic/fixnum-debug
   "Retrying an atomic fixnum operation due to contention.
  fixnum: ~e
  operation: ~a"
   num enclosing-function-name))

(struct atomic-fixnum ([get #:mutable] name)
  #:property prop:object-name (struct-field-index name)
  #:constructor-name constructor:atomic-fixnum
  #:authentic)

(define (atomic-fixnum-compare-and-set! num expected replacement)
  (unsafe-struct*-cas! num 0 expected replacement))

(define (make-atomic-fixnum initial-value #:name [name #false])
  (constructor:atomic-fixnum initial-value name))

(define (atomic-fixnum-compare-and-add! num expected amount)
  (atomic-fixnum-compare-and-set! num expected (fx+ expected amount)))

(define/name (atomic-fixnum-compare-and-exchange! num expected replacement)
  (guarded-block
    (define x (atomic-fixnum-get num))
    (guard (eq? x expected) else x)
    (guard (atomic-fixnum-compare-and-set! num expected replacement) else
      (log-atomic-fixnum-contention num)
      (atomic-fixnum-compare-and-exchange! num expected replacement))
    x))

(define/name (atomic-fixnum-get-then-set! num replacement)
  (guarded-block
    (define x (atomic-fixnum-get num))
    (guard (atomic-fixnum-compare-and-set! num x replacement) else
      (log-atomic-fixnum-contention num)
      (atomic-fixnum-get-then-set! num replacement))
    x))

(define/name (atomic-fixnum-add! num amount)
  (define x (atomic-fixnum-get num))
  (unless (atomic-fixnum-compare-and-set! num x (fx+ x amount))
    (log-atomic-fixnum-contention num)
    (atomic-fixnum-add! num amount)))

(define/name (atomic-fixnum-get-then-add! num amount)
  (guarded-block
    (define x (atomic-fixnum-get num))
    (guard (atomic-fixnum-compare-and-add! num x amount) else
      (log-atomic-fixnum-contention num)
      (atomic-fixnum-get-then-add! num amount))
    x))

(define/name (atomic-fixnum-add-then-get! num amount)
  (guarded-block
    (define x (atomic-fixnum-get num))
    (define x* (fx+ x amount))
    (guard (atomic-fixnum-compare-and-set! num x x*) else
      (log-atomic-fixnum-contention num)
      (atomic-fixnum-add-then-get! num amount))
    x*))

(define/name (atomic-fixnum-update! num updater)
  (define x (atomic-fixnum-get num))
  (unless (atomic-fixnum-compare-and-set! num x (updater x))
    (log-atomic-fixnum-contention num)
    (atomic-fixnum-update! num updater)))

(define/name (atomic-fixnum-get-then-update! num updater)
  (guarded-block
    (define x (atomic-fixnum-get num))
    (guard (atomic-fixnum-compare-and-set! num x (updater x)) else
      (log-atomic-fixnum-contention num)
      (atomic-fixnum-get-then-update! num updater))
    x))

(define/name (atomic-fixnum-update-then-get! num updater)
  (guarded-block
    (define x (atomic-fixnum-get num))
    (define x* (updater x))
    (guard (atomic-fixnum-compare-and-set! num x x*) else
      (log-atomic-fixnum-contention num)
      (atomic-fixnum-update-then-get! num updater))
    x*))

(module+ test

  (define (range-sum lower upper)
    (/ (- (* upper (add1 upper)) (* lower (sub1 lower))) 2))

  (test-case (name-string range-sum)
    (check-equal? (range-sum 1 3) 6)
    (check-equal? (range-sum 1 10) 55)
    (check-equal? (range-sum 5 15) 110))

  (define (call/contention #:threads num-threads #:calls num-calls thunk)
    (define threads
      (for/list ([_ (in-range num-threads)])
        (thread (λ () (for ([_ (in-range num-calls)]) (thunk))))))
    (for ([thd (in-list threads)]) (thread-wait thd)))
    
  (define-simple-macro
    (with-contention #:threads threads:expr #:calls calls:expr body:expr ...+)
    (call/contention #:threads threads #:calls calls (λ () body ...)))
  
  (test-case (name-string atomic-fixnum-compare-and-set!)
    (define num (make-atomic-fixnum 0))
    (check-true (atomic-fixnum-compare-and-set! num 0 5))
    (check-equal? (atomic-fixnum-get num) 5)
    (check-false (atomic-fixnum-compare-and-set! num 0 42))
    (check-equal? (atomic-fixnum-get num) 5))

  (test-case (name-string atomic-fixnum-add!)
    (define num (make-atomic-fixnum 0))
    (with-contention #:threads 1000 #:calls 1000 (atomic-fixnum-add! num 1))
    (check-equal? (atomic-fixnum-get num) 1000000))

  (test-case (name-string atomic-fixnum-get-then-add!)
    (define num (make-atomic-fixnum 0))
    (define total (make-atomic-fixnum 0))
    (with-contention #:threads 1000 #:calls 1000
      (atomic-fixnum-add! total (atomic-fixnum-get-then-add! num 1)))
    (check-equal? (atomic-fixnum-get num) 1000000)
    (check-equal? (atomic-fixnum-get total) (range-sum 0 999999)))

  (test-case (name-string atomic-fixnum-add-then-get!)
    (define num (make-atomic-fixnum 0))
    (define total (make-atomic-fixnum 0))
    (with-contention #:threads 1000 #:calls 1000
      (atomic-fixnum-add! total (atomic-fixnum-add-then-get! num 1)))
    (check-equal? (atomic-fixnum-get num) 1000000)
    (check-equal? (atomic-fixnum-get total) (range-sum 1 1000000)))

  (test-case (name-string atomic-fixnum-update!)
    (define num (make-atomic-fixnum 0))
    (with-contention #:threads 1000 #:calls 1000
      (atomic-fixnum-update! num add1))
    (check-equal? (atomic-fixnum-get num) 1000000))

  (test-case (name-string atomic-fixnum-get-then-update!)
    (define num (make-atomic-fixnum 0))
    (define total (make-atomic-fixnum 0))
    (with-contention #:threads 1000 #:calls 1000
      (atomic-fixnum-add! total (atomic-fixnum-get-then-update! num add1)))
    (check-equal? (atomic-fixnum-get num) 1000000)
    (check-equal? (atomic-fixnum-get total) (range-sum 0 999999)))

  (test-case (name-string atomic-fixnum-update-then-get!)
    (define num (make-atomic-fixnum 0))
    (define total (make-atomic-fixnum 0))
    (with-contention #:threads 1000 #:calls 1000
      (atomic-fixnum-add! total (atomic-fixnum-update-then-get! num add1)))
    (check-equal? (atomic-fixnum-get num) 1000000)
    (check-equal? (atomic-fixnum-get total) (range-sum 1 1000000))))
