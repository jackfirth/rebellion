#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [synchronized-sorted-set? (-> any/c boolean?)]
  [synchronized-sorted-set (-> mutable-sorted-set? synchronized-sorted-set?)]
  [synchronized-sorted-set-lock (-> synchronized-sorted-set? read-write-lock?)]))


(require racket/generic
         rebellion/collection/private/sorted-set-interface
         rebellion/concurrency/lock
         (submod rebellion/collection/private/sorted-set-interface private-for-rebellion-only))


;@----------------------------------------------------------------------------------------------------


(define (synchronized-sorted-set delegate)
  (make-synchronized-sorted-set delegate (make-read-write-lock)))


(struct synchronized-sorted-set abstract-mutable-sorted-set (delegate-set lock)

  #:constructor-name make-synchronized-sorted-set

  #:omit-define-syntaxes

  #:methods gen:sorted-set

  [(define (get-delegate this)
     (synchronized-sorted-set-delegate-set this))

   (define (synchronize this thunk)
     (lock! (read-write-lock-read-lock (synchronized-sorted-set-lock this)) thunk))

   (define/generic generic-in-sorted-set in-sorted-set)
   (define/generic generic-sorted-set-empty? sorted-set-empty?)
   (define/generic generic-sorted-set-size sorted-set-size)
   (define/generic generic-sorted-set-comparator sorted-set-comparator)
   (define/generic generic-sorted-set-contains? sorted-set-contains?)
   (define/generic generic-sorted-set-contains-any? sorted-set-contains-any?)
   (define/generic generic-sorted-set-contains-all? sorted-set-contains-all?)
   (define/generic generic-sorted-set-contains-none? sorted-set-contains-none?)
   (define/generic generic-sorted-set-least-element sorted-set-least-element)
   (define/generic generic-sorted-set-greatest-element sorted-set-greatest-element)
   (define/generic generic-sorted-set-element-less-than sorted-set-element-less-than)
   (define/generic generic-sorted-set-element-greater-than sorted-set-element-greater-than)
   (define/generic generic-sorted-set-element-at-least sorted-set-element-at-least)
   (define/generic generic-sorted-set-element-at-most sorted-set-element-at-most)
   (define/generic generic-sorted-subset sorted-subset)
   (define/generic generic-sorted-set-reverse sorted-set-reverse)

   (define (in-sorted-set this #:descending? [descending? #false])
     (generic-in-sorted-set (get-delegate this) #:descending? descending?))

   (define (sorted-set-empty? this)
     (synchronize this (λ () (generic-sorted-set-empty? (get-delegate this)))))

   (define (sorted-set-size this)
     (synchronize this (λ () (generic-sorted-set-size (get-delegate this)))))

   (define (sorted-set-comparator this)
     (synchronize this (λ () (generic-sorted-set-comparator (get-delegate this)))))

   (define (sorted-set-contains? this value)
     (synchronize this (λ () (generic-sorted-set-contains? (get-delegate this) value))))

   (define (sorted-set-contains-any? this values)
     (synchronize this (λ () (generic-sorted-set-contains-any? (get-delegate this) values))))

   (define (sorted-set-contains-all? this values)
     (synchronize this (λ () (generic-sorted-set-contains-all? (get-delegate this) values))))

   (define (sorted-set-contains-none? this values)
     (synchronize this (λ () (generic-sorted-set-contains-none? (get-delegate this) values))))

   (define (sorted-set-least-element this)
     (synchronize this (λ () (generic-sorted-set-least-element (get-delegate this)))))

   (define (sorted-set-greatest-element this)
     (synchronize this (λ () (generic-sorted-set-greatest-element (get-delegate this)))))

   (define (sorted-set-element-less-than this upper-bound)
     (synchronize this (λ () (generic-sorted-set-element-less-than (get-delegate this) upper-bound))))

   (define (sorted-set-element-greater-than this lower-bound)
     (synchronize
      this (λ () (generic-sorted-set-element-greater-than (get-delegate this) lower-bound))))

   (define (sorted-set-element-at-most this upper-bound)
     (synchronize this (λ () (generic-sorted-set-element-at-most (get-delegate this) upper-bound))))
   
   (define (sorted-set-element-at-least this lower-bound)
     (synchronize this (λ () (generic-sorted-set-element-at-least (get-delegate this) lower-bound))))

   (define (sorted-subset this element-range)
     (make-synchronized-sorted-set
      (generic-sorted-subset (get-delegate this) element-range)
      (synchronized-sorted-set-lock this)))

   (define (sorted-set-reverse this)
     (make-synchronized-sorted-set
      (generic-sorted-set-reverse (get-delegate this))
      (synchronized-sorted-set-lock this)))]

  #:methods gen:mutable-sorted-set

  [(define (get-delegate this)
     (synchronized-sorted-set-delegate-set this))

   (define (synchronize this thunk)
     (lock! (read-write-lock-write-lock (synchronized-sorted-set-lock this)) thunk))

   (define/generic generic-sorted-set-add! sorted-set-add!)
   (define/generic generic-sorted-set-add-all! sorted-set-add-all!)
   (define/generic generic-sorted-set-remove! sorted-set-remove!)
   (define/generic generic-sorted-set-remove-all! sorted-set-remove-all!)
   (define/generic generic-sorted-set-clear! sorted-set-clear!)

   (define (sorted-set-add! this element)
     (synchronize this (λ () (generic-sorted-set-add! (get-delegate this) element))))

   (define (sorted-set-add-all! this elements)
     (synchronize this (λ () (generic-sorted-set-add-all! (get-delegate this) elements))))

   (define (sorted-set-remove! this element)
     (synchronize this (λ () (generic-sorted-set-remove! (get-delegate this) element))))

   (define (sorted-set-remove-all! this elements)
     (synchronize this (λ () (generic-sorted-set-remove-all! (get-delegate this) elements))))
   
   (define (sorted-set-clear! this)
     (synchronize this (λ () (generic-sorted-set-clear! (get-delegate this)))))])
