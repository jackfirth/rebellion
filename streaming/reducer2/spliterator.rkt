#lang racket/base


(require racket/contract/base)


(provide
 gen:spliterator
 (contract-out
  [spliterator? (-> any/c boolean?)]
  [spliterator-try-next (-> spliterator? (values any/c boolean?))]
  [spliterator-split (-> spliterator? (values spliterator? (or/c spliterator? #false)))]
  [spliterator-size-estimate (-> spliterator? (or/c exact-nonnegative-integer? +inf.0 #false))]
  [spliterator-ordered? (-> spliterator? boolean?)]
  [spliterator-fold (-> spliterator? (-> any/c any/c any/c) any/c any/c)]
  [spliterator-fold-with-break (-> spliterator? (-> any/c any/c (values any/c boolean?)) any/c any/c)]
  [spliterator-for-each (-> spliterator? (-> any/c any/c) void?)]
  [spliterator-for-each-with-break (-> spliterator? (-> any/c boolean?) void?)]))


(module+ unchecked
  (provide gen:spliterator
           spliterator?
           spliterator-try-next
           spliterator-split
           spliterator-size-estimate
           spliterator-ordered?
           spliterator-fold
           spliterator-fold-with-break
           spliterator-for-each
           spliterator-for-each-with-break))


(require racket/generic)


;@----------------------------------------------------------------------------------------------------


(define-generics spliterator
  (spliterator-try-next spliterator)
  (spliterator-split spliterator)
  (spliterator-size-estimate spliterator)
  (spliterator-ordered? spliterator)
  (spliterator-fold spliterator folder init)
  (spliterator-fold-with-break spliterator folder init)
  (spliterator-for-each spliterator action)
  (spliterator-for-each-with-break spliterator action)

  #:requires [spliterator-try-next]

  #:fallbacks
  [(define/generic generic-spliterator-try-next spliterator-try-next)

   (define (spliterator-split this)
     (values this #false))

   (define (spliterator-size-estimate this)
     #false)

   (define (spliterator-ordered? this)
     #true)

   (define (spliterator-fold this folder init)
     (let loop ([this this] [state init])
       (define-values (elem next-this) (generic-spliterator-try-next this))
       (if next-this
           (loop next-this (folder state elem))
           state)))

   (define (spliterator-fold-with-break this folder init)
     (let loop ([this this] [state init])
       (define-values (elem next-this) (generic-spliterator-try-next this))
       (cond
         [next-this
          (define-values (next-state continue?) (folder state elem))
          (if continue? (loop next-this next-state) next-state)]
         [else state])))

   (define (spliterator-for-each this action)
     (let loop ([this this])
       (define-values (elem next-this) (generic-spliterator-try-next this))
       (when next-this
         (action elem)
         (loop next-this))))

   (define (spliterator-for-each-with-break this action)
     (let loop ([this this])
       (define-values (elem next-this) (generic-spliterator-try-next this))
       (when next-this
         (when (action elem)
           (loop next-this)))))])
