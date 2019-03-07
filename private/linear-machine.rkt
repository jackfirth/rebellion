#lang racket/base

(require rebellion/record
         rebellion/variant
         racket/set)

(define empty-set (set))

(struct linear-result (state outputs))

(struct linear-action (source-states target-states inputs outputs function)
  #:constructor-name plain-make-linear-action
  #:omit-define-syntaxes)

(define (make-linear-action #:source-states source-states
                            #:target-states target-states
                            #:inputs inputs
                            #:outputs outputs
                            function)
  (plain-make-linear-action source-states
                            target-states
                            inputs
                            outputs
                            function))

(struct linear-machine (states values starter finisher actions)
  #:constructor-name plain-make-linear-machine
  #:omit-define-syntaxes)

(define (make-linear-machine #:states states
                             #:values values
                             #:starter starter
                             #:finisher finisher
                             #:actions actions)
  (plain-make-linear-machine states values starter finisher actions))

(define (fold-machine f init)
  (make-linear-machine
   #:states (set '#:folding '#:done)
   #:values (set '#:element '#:result)
   #:starter (variant #:folding (λ () init))
   #:finisher (variant #:done void)
   #:actions
   (record
    #:consume (make-linear-action
               #:source-states (set '#:folding)
               #:target-states (set '#:folding)
               #:inputs (set '#:element)
               #:outputs empty-set
               (λ (state v)
                 (linear-result (variant #:folding (f state v)))))
    #:finish (make-linear-action
              #:source-states (set '#:folding)
              #:target-states (set '#:done)
              #:inputs empty-set
              #:outputs (set '#:result)
              (λ (state)
                (linear-result (variant #:done #f)
                               (record #:result state)))))))
