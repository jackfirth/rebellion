#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [transactional-box? predicate/c]
  [make-transactional-box (-> any/c transactional-box?)]
  [transactional-box-get (-> transactional-box? any/c)]
  [transactional-box-set! (-> transactional-box? any/c void?)]
  [transactional-box-update! (-> transactional-box? (-> any/c any/c) void?)]
  [run-in-transaction (-> (-> any) any)]))


(require racket/match
         rebellion/base/option
         rebellion/concurrency/atomic/fixnum
         rebellion/concurrency/lock
         rebellion/private/guarded-block
         rebellion/private/static-name)


(define global-version (make-atomic-fixnum 0))

(define transactional-lock-count (make-atomic-fixnum 0))

(struct transactional-lock (rw-lock id version-box)
  #:constructor-name constructor:transactional-lock)

(define (make-transactional-lock)
  (define id (atomic-fixnum-get-then-add! transactional-lock-count 1))
  (define version (make-atomic-fixnum (atomic-fixnum-get global-version)))
  (constructor:transactional-lock (make-read-write-lock) id version))

(define (transactional-lock-version lock)
  (atomic-fixnum-get (transactional-lock-version-box lock)))

(define (transactional-lock-try-acquire-for-read lock #:success success-thunk #:failure failure-thunk)
  (match (lock-try-acquire-in
          (read-write-lock-read-lock (transactional-lock-rw-lock lock))
          success-thunk)
    [(present result) result]
    [(== absent) (failure-thunk)]))


(struct transactional-box ([current-value #:mutable] lock)
  #:constructor-name constructor:transactional-box)


(define (make-transactional-box initial-value)
  (constructor:transactional-box initial-value (make-transactional-lock)))


(define current-transaction (make-parameter #false))


(struct transaction
  (owner abort-continuation start-version [read-set #:mutable] [write-set #:mutable])
  #:constructor-name constructor:transaction)


(define (make-transaction abort-continuation)
  (constructor:transaction
   (current-thread)
   abort-continuation
   (atomic-fixnum-get global-version)
   '()
   '()))


(define (transaction-abort! transaction)
  (define abort (transaction-abort-continuation transaction))
  (abort absent))


(define/guard (transaction-add-read! transaction lock action-thunk)
  (transactional-lock-try-acquire-for-read
   lock
   #:success
   (λ ()
     (guarded-block
       (guard (>= (transaction-start-version transaction) (transactional-lock-version lock)) else
         (transaction-abort! transaction))
       (define result (action-thunk))
       (set-transaction-read-set! transaction (cons lock (transaction-read-set transaction)))
       result))
   #:failure (λ () (transaction-abort! transaction))))


(define (transaction-lock-write-set! transaction)
  (void))


(define (transaction-validate-read-set! transaction)
  (void))


(define (transaction-commit-write-set! transaction)
  (void))


(define (transaction-unlock-write-set! transaction)
  (void))


(define (transaction-write-set-contains? transaction lock)
  (void))


(define (transaction-write-set-ref transaction lock)
  (void))


(define (check-transaction-owned-by-current-thread transaction)
  (void))


(define (run-in-transaction thunk)
  (define transaction-result
    (let/ec abort-continuation
      (parameterize ([current-transaction (make-transaction abort-continuation)])
        (define result (thunk))
        (transaction-lock-write-set! (current-transaction))
        (atomic-fixnum-add! global-version 1)
        (transaction-validate-read-set! (current-transaction))
        (transaction-commit-write-set! (current-transaction))
        (transaction-unlock-write-set! (current-transaction))
        (present result))))
  (match transaction-result
    [(present result) result]
    [(== absent) (run-in-transaction thunk)]))


(define/guard (transactional-box-get box)
  (define transaction (current-transaction))
  (guard transaction else
    (raise-arguments-error
     (name transactional-box-get)
     "cannot open transactional box, not currently within transaction"
     "box" box
     "thread" (current-thread)))
  (check-transaction-owned-by-current-thread transaction)
  (define lock (transactional-box-lock box))
  (define current-value (transactional-box-current-value box))
  (transaction-add-read! transaction lock)
  (if (transaction-write-set-contains? transaction lock)
      (transaction-write-set-ref transaction lock)
      current-value))


(define/guard (transactional-box-set! box new-value)
  (define transaction (current-transaction))
  (guard transaction else
    (raise-arguments-error
     (name transactional-box-set!)
     "cannot update transactional box, not currently within transaction"
     "box" box
     "thread" (current-thread)
     "new value" new-value))
  (check-transaction-owned-by-current-thread transaction)
  (void))


(define (transactional-box-update! box updater)
  (transactional-box-set! box (updater (transactional-box-get box))))
