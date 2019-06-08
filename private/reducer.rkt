#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [reduce (-> reducer? any/c ... any/c)]
  [reducer? predicate/c]
  [into-count reducer?]))

(require racket/list
         rebellion/custom-write
         rebellion/type/record)

;@------------------------------------------------------------------------------

(define (make-reducer-properties descriptor)
  (define type (record-descriptor-type descriptor))
  (define equal+hash (make-record-equal+hash descriptor))
  (define custom-write (make-named-object-custom-write (record-type-name type)))
  (define object-name (index-of (record-type-fields type) '#:name))
  (list (cons prop:equal+hash equal+hash)
        (cons prop:custom-write custom-write)
        (cons prop:object-name object-name)))

(define-record-type reducer (starter consumer closer finisher name)
  #:constructor-name constructor:reducer
  #:property-maker make-reducer-properties)

(define (make-reducer #:starter starter
                      #:consumer consumer
                      #:closer closer
                      #:finisher finisher
                      #:name [name #f])
  (constructor:reducer #:starter starter
                       #:consumer consumer
                       #:closer closer
                       #:finisher finisher
                       #:name name))

(define into-count
  (make-reducer 

(define (reduce red . vs)
  (define starter (reducer-starter red))
  (define consumer (reducer-consumer red))
  (define closer (reducer-closer red))
  (define finisher (reducer-finisher red))
  (let loop ([state (starter)] [vs vs])
    (case (variant-tag state)
      [(#:consuming)
       (if (empty? vs)
           (closer 
           (loop (consumer state (first vs)) (rest vs))]
      [(#:done)