#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [lock? (-> any/c boolean?)]
  [lock! (-> lock? (-> any) any)]
  [lock-acquire! (-> lock? void?)]
  [lock-release! (-> lock? void?)]
  [read-write-lock? (-> any/c boolean?)]
  [read-write-lock-read-lock (-> read-write-lock? lock?)]
  [read-write-lock-write-lock (-> read-write-lock? lock?)]
  [make-lock (->* () (#:reentrant? boolean?) lock?)]
  [make-read-write-lock (->* () (#:reentrant? boolean?) read-write-lock?)]))


(require racket/generic
         rebellion/private/static-name)


;@----------------------------------------------------------------------------------------------------


(define-generics lock

  (lock! lock thunk)
  (lock-acquire! lock)
  (lock-release! lock)

  #:fallbacks

  [(define/generic generic-lock-acquire! lock-acquire!)
   (define/generic generic-lock-release! lock-release!)
   
   (define (lock! this thunk)
     (dynamic-wind
      (λ () (generic-lock-acquire! this))
      (λ () (call-with-continuation-barrier thunk))
      (λ () (generic-lock-release! this))))])


(define (make-lock #:reentrant? [reentrant? #true])
  (if reentrant? (make-reentrant-lock) (make-non-reentrant-lock)))


(define (make-reentrant-lock)
  (reentrant-lock (make-primitive-semaphore-lock) (make-primitive-semaphore-lock) 0 #false))


(define (make-non-reentrant-lock)
  (non-reentrant-lock (make-primitive-semaphore-lock) (make-primitive-semaphore-lock) #false))


(define (make-primitive-semaphore-lock)
  (primitive-semaphore-lock (make-semaphore 1)))


;; Not reentrant: deadlocks if acquire! is called twice in a row, and has buggy behavior if release!
;; is called twice in a row. This implementation should not be exposed directly to clients. Instead,
;; clients should use either the reentrant-lock or the non-reentrant-lock implementations. Those
;; implementations are what make-lock returns. This primitive lock is used as a building block for the
;; more robust implementations.
(struct primitive-semaphore-lock (semaphore)

  #:methods gen:lock

  [(define (lock-acquire! this)
     (semaphore-wait (primitive-semaphore-lock-semaphore this)))

   (define (lock-release! this)
     (semaphore-post (primitive-semaphore-lock-semaphore this)))

   (define (lock! this thunk)
     (call-with-semaphore (primitive-semaphore-lock-semaphore this) thunk))])


;; A non-reentrant wrapper around any lock. Raises errors on attempted reentrant locking.
(struct non-reentrant-lock (control-lock delegate-lock [owner #:mutable])

  #:methods gen:lock

  [(define/generic generic-lock-acquire! lock-acquire!)
   (define/generic generic-lock-release! lock-release!)
   (define/generic generic-lock! lock!)

   (define (lock-acquire! this)
     (generic-lock!
      (reentrant-lock-control-lock this)
      (λ ()
        (when (equal? (non-reentrant-lock-owner this) (current-thread))
          (raise-arguments-error
           (name lock-acquire!)
           "cannot acquire lock, the current thread already holds it"
           "lock" this
           "current thread" (current-thread)))
        (generic-lock-acquire! (non-reentrant-lock-delegate-lock this))
        (set-reentrant-lock-owner! this (current-thread)))))

   (define (lock-release! this)
     (generic-lock!
      (reentrant-lock-control-lock this)
      (λ ()
        (generic-lock-release! (reentrant-lock-delegate-lock this))
        (set-reentrant-lock-owner! this #false))))])


;; A reentrant wrapper around any non-reentrant lock.
(struct reentrant-lock (control-lock delegate-lock [acquisition-count #:mutable] [owner #:mutable])

  #:methods gen:lock

  [(define/generic generic-lock-acquire! lock-acquire!)
   (define/generic generic-lock-release! lock-release!)
   (define/generic generic-lock! lock!)

   (define (lock-acquire! this)
     (generic-lock!
      (reentrant-lock-control-lock this)
      (λ ()
        (unless (equal? (reentrant-lock-owner this) (current-thread))
          (generic-lock-acquire! (reentrant-lock-delegate-lock this))
          (set-reentrant-lock-owner! this (current-thread)))
        (set-reentrant-lock-acquisition-count! this (add1 (reentrant-lock-acquisition-count this))))))

   (define (lock-release! this)
     (generic-lock!
      (reentrant-lock-control-lock this)
      (λ ()
        (set-reentrant-lock-acquisition-count! this (sub1 (reentrant-lock-acquisition-count this)))
        (when (zero? (reentrant-lock-acquisition-count this))
          (generic-lock-release! (reentrant-lock-delegate-lock this))
          (set-reentrant-lock-owner! this #false)))))])


(define (make-read-write-lock #:reentrant? [reentrant? #true])
  (read-write-lock 0 (make-primitive-semaphore-lock) (make-lock #:reentrant? reentrant?)))


(struct read-write-lock ([reader-count #:mutable] reader-count-lock write-lock))


(struct read-write-lock-read-lock (rw-lock)

  #:methods gen:lock

  [(define/generic generic-lock-acquire! lock-acquire!)
   (define/generic generic-lock-release! lock-release!)
   (define/generic generic-lock! lock!)
   
   (define (lock-acquire! this)
     (define rw-lock (read-write-lock-read-lock-rw-lock this))
     (generic-lock!
      (read-write-lock-reader-count-lock rw-lock)
      (λ ()
        (set-read-write-lock-reader-count! rw-lock (add1 (read-write-lock-reader-count rw-lock)))
        (when (equal? (read-write-lock-reader-count rw-lock) 1)
          (generic-lock-acquire! (read-write-lock-write-lock rw-lock))))))

   (define (lock-release! this)
     (define rw-lock (read-write-lock-read-lock-rw-lock this))
     (generic-lock!
      (read-write-lock-reader-count-lock rw-lock)
      (λ ()
        (set-read-write-lock-reader-count! rw-lock (sub1 (read-write-lock-reader-count rw-lock)))
        (when (zero? (read-write-lock-reader-count rw-lock))
          (generic-lock-release! (read-write-lock-write-lock rw-lock))))))])
