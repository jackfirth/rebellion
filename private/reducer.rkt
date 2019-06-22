#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [reduce (-> reducer? any/c ... any/c)]
  [reducer? predicate/c]
  [into-count reducer?]))

(require racket/list
         rebellion/collection/entry
         rebellion/collection/keyset
         rebellion/custom-write
         rebellion/variant
         rebellion/type/record)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define (make-reducer-properties descriptor)
  (define type (record-descriptor-type descriptor))
  (define equal+hash (make-record-equal+hash descriptor))
  (define custom-write (make-named-object-custom-write (record-type-name type)))
  (define object-name (keyset-index-of (record-type-fields type) '#:name))
  (list (cons prop:equal+hash equal+hash)
        (cons prop:custom-write custom-write)
        (cons prop:object-name object-name)))

(define-record-type reducer (starter consumer finisher early-finisher name)
  #:constructor-name constructor:reducer
  #:property-maker make-reducer-properties)

(define (make-reducer #:starter starter
                      #:consumer consumer
                      #:finisher finisher
                      #:early-finisher early-finisher
                      #:name [name #f])
  (constructor:reducer #:starter starter
                       #:consumer consumer
                       #:finisher finisher
                       #:early-finisher early-finisher
                       #:name name))

(define (make-nonterminating-reducer #:starter starter
                                     #:consumer consumer
                                     #:finisher finisher
                                     #:name [name #f])
  (make-reducer #:starter (λ () (variant #:consume (starter)))
                #:consumer (λ (state v) (variant #:consume (consumer state v)))
                #:finisher finisher
                #:early-finisher values
                #:name name))

(define (into-fold op init #:name [name #f])
  (make-nonterminating-reducer #:starter (λ () init)
                               #:consumer op
                               #:finisher values
                               #:name name))

(define (reduce red . vs)
  (define starter (reducer-starter red))
  (define consumer (reducer-consumer red))
  (define finisher (reducer-finisher red))
  (define early-finisher (reducer-early-finisher red))
  (let loop ([step (starter)] [vs vs])
    (define state (variant-value step))
    (case (variant-tag step)
      [(#:consume)
       (if (empty? vs)
           (finisher state)
           (loop (consumer state (first vs)) (rest vs)))]
      [(#:early-finish)
       (early-finisher state)])))

(define (bireduce red . entries)
  (define starter (reducer-starter red))
  (define consumer (reducer-consumer red))
  (define finisher (reducer-finisher red))
  (define early-finisher (reducer-early-finisher red))
  (let loop ([step (starter)] [entries entries])
    (define state (variant-value step))
    (case (variant-tag step)
      [(#:consume)
       (cond
         [(empty? entries) (finisher state)]
         [else
          (define k (first entries))
          (define v (second entries))
          (define e (entry k v))
          (loop (consumer state e) (drop entries 2))])]
      [(#:early-finish)
       (early-finisher state)])))

(define into-count (into-fold (λ (n _) (add1 n)) 0 #:name 'into-count))

(module+ test
  (test-case "into-count"
    (check-equal? (reduce into-count) 0)
    (check-equal? (reduce into-count 'a) 1)
    (check-equal? (reduce into-count 'a 'b 'c 'd 'e) 5)))

(define (into-nth n #:or-else [default #f])
  (make-reducer
   #:starter (λ () (variant #:consume n))
   #:consumer (λ (countdown v)
                (if (zero? countdown)
                    (variant #:early-finish v)
                    (variant #:consume (sub1 countdown))))
   #:finisher (λ (_) default)
   #:early-finisher values
   #:name 'into-nth))

(module+ test
  (test-case "into-nth"
    (check-equal? (reduce (into-nth 0) 'a 'b 'c 'd) 'a)
    (check-equal? (reduce (into-nth 2) 'a 'b 'c 'd) 'c)
    (check-equal? (reduce (into-nth 5) 'a 'b 'c 'd) #f)
    (check-equal? (reduce (into-nth 5 #:or-else 'FOO) 'a 'b 'c 'd) 'FOO)))

(define (reducer-map red
                     #:domain [f values]
                     #:range [g values]
                     #:name [name #f])
  (define starter (reducer-starter red))
  (define consumer (reducer-consumer red))
  (define finisher (reducer-finisher red))
  (define early-finisher (reducer-early-finisher red))
  (make-reducer #:starter starter
                #:consumer (λ (state v) (consumer state (f v)))
                #:finisher (λ (state) (g (finisher state)))
                #:early-finisher (λ (state) (g (early-finisher state)))
                #:name name))

(define (list-push lst v) (cons v lst))

(define into-reversed-list
  (into-fold list-push '() #:name 'into-reversed-list))

(define into-list
  (reducer-map into-reversed-list #:range reverse #:name 'into-list))

(module+ test
  (test-case "into-reversed-list"
    (check-equal? (reduce into-reversed-list) '())
    (check-equal? (reduce into-reversed-list 1 2 3 4 5) (list 5 4 3 2 1)))
  (test-case "into-list"
    (check-equal? (reduce into-list) '())
    (check-equal? (reduce into-list 1 2 3 4 5) (list 1 2 3 4 5))))

(define (hash-add-entry h e)
  (hash-set h (entry-key e) (entry-value e)))

(define into-hash (into-fold hash-add-entry (hash)))

(module+ test
  (test-case "into-hash"
    (check-equal? (bireduce into-hash 'a 1 'b 2 'c 3 'b 4)
                  (hash 'a 1 'b 4 'c 3))))
