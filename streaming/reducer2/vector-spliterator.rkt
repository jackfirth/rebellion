#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  #:unprotected-submodule unchecked
  [vector-spliterator? (-> any/c boolean?)]
  [vector-spliterator
   (-> vector? exact-nonnegative-integer? exact-nonnegative-integer? vector-spliterator?)]))


(require rebellion/streaming/reducer2/spliterator)


;@----------------------------------------------------------------------------------------------------


(struct vector-spliterator (base-vector start end)
  
  #:methods gen:spliterator

  [(define (spliterator-try-next this)
     (define start (vector-spliterator-start this))
     (define end (vector-spliterator-end this))
     (cond
       [(< start end)
        (define base-vector (vector-spliterator-base-vector this))
        (values (vector-ref base-vector start)
                (vector-spliterator base-vector (add1 start) end))]
       [else (values #false #false)]))

   (define (spliterator-split this)
     (define base-vector (vector-spliterator-base-vector this))
     (define start (vector-spliterator-start this))
     (define end (vector-spliterator-end this))
     (cond
       [(< start (sub1 end))
        (define midpoint (truncate (/ (+ start end) 2)))
        (values (vector-spliterator base-vector start midpoint)
                (vector-spliterator base-vector midpoint end))]
       [else (values this #false)]))

   (define (spliterator-size-estimate this)
     (define start (vector-spliterator-start this))
     (define end (vector-spliterator-end this))
     (- end start))

   (define (spliterator-fold this folder init)
     (define base-vector (vector-spliterator-base-vector this))
     (define start (vector-spliterator-start this))
     (define end (vector-spliterator-end this))
     (for/fold ([state init])
               ([v (in-vector base-vector start end)])
       (folder state v)))

   (define (spliterator-fold-with-break this folder init)
     (define base-vector (vector-spliterator-base-vector this))
     (define start (vector-spliterator-start this))
     (define end (vector-spliterator-end this))
     (for/fold ([state init])
               ([v (in-vector base-vector start end)])
       (define-values (new-state continue?) (folder state v))
       #:final (not continue?)
       new-state))])
