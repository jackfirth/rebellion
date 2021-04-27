#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [lock? predicate/c]
  [lock-acquire! (-> lock? void?)]
  [lock-release! (-> lock? void?)]
  [lock-acquire-in (-> lock? (-> any) any)]
  [lock-try-acquire! (-> lock? boolean?)]
  [lock-try-acquire-in (-> lock? (-> any/c) option?)]
  [make-lock (-> lock?)]
  [read-write-lock? predicate/c]
  [read-write-lock-read-lock (-> read-write-lock? lock?)]
  [read-write-lock-write-lock (-> read-write-lock? lock?)]
  [make-read-write-lock (-> read-write-lock?)]))


(require rebellion/base/option)


(struct lock
  (acquire-thunk acquire-in-function release-thunk try-acquire-thunk try-acquire-in-function)
  #:constructor-name constructor:lock)


(define (lock-acquire! lock)
  ((lock-acquire-thunk lock)))


(define (lock-try-acquire! lock)
  ((lock-try-acquire-thunk lock)))


(define (lock-release! lock)
  ((lock-release-thunk lock)))


(define (lock-acquire-in lock thunk)
  ((lock-acquire-in-function lock) thunk))


(define (lock-try-acquire-in lock thunk)
  ((lock-try-acquire-in-function lock) thunk))


(define (make-lock)
  (define lock-semaphore (make-semaphore 1))

  (define (acquire!)
    (semaphore-wait lock-semaphore))
  
  (define (try-acquire!)
    (not (not (sync/timeout 0 (semaphore-peek-evt lock-semaphore)))))

  (define (release!)
    (semaphore-post lock-semaphore))
  
  (define (acquire-in thunk)
    (call-with-semaphore lock-semaphore thunk))

  (define (try-acquire-in thunk)
    (call-with-semaphore lock-semaphore (λ () (present (thunk))) (λ () absent)))

  (constructor:lock acquire! try-acquire! release! acquire-in try-acquire-in))


(struct read-write-lock (read-lock write-lock)
  #:constructor-name constructor:read-write-lock)


(define (make-read-write-lock)
  (define blocking-reader-count 0)
  (define reader-count-lock (make-lock))
  (define write-lock (make-lock))

  (define (read-lock-acquire!)
    (lock-acquire-in
     reader-count-lock
     (λ ()
       (set! blocking-reader-count (add1 blocking-reader-count))
       (when (equal? blocking-reader-count 1)
         (lock-acquire! write-lock)))))

  (define (read-lock-try-acquire!)
    (define successful? (lock-try-acquire! reader-count-lock))
    (cond
      [successful?
       (set! blocking-reader-count (add1 blocking-reader-count))
       (when (equal? blocking-reader-count 1)
         (lock-acquire! write-lock))
       #true]
      [else #false]))

  (define (read-lock-release!)
    (lock-acquire-in
     reader-count-lock
     (λ ()
       (set! blocking-reader-count (sub1 blocking-reader-count))
       (when (zero? blocking-reader-count)
         (lock-release! write-lock)))))

  (define (read-lock-acquire-in thunk)
    (dynamic-wind
     read-lock-acquire!
     (λ () (call-with-continuation-barrier thunk))
     read-lock-release!))

  (define (read-lock-try-acquire-in thunk)
    (define success? (read-lock-try-acquire!))
    (cond
      [success?
       (present
        (dynamic-wind
         void
         (λ () (call-with-continuation-barrier thunk))
         read-lock-release!))]
      [else absent]))

  (define read-lock
    (constructor:lock
     read-lock-acquire!
     read-lock-try-acquire!
     read-lock-release!
     read-lock-acquire-in
     read-lock-try-acquire-in))
  (constructor:read-write-lock read-lock write-lock))
