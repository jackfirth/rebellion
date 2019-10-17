#lang racket/base

(require racket/contract/base)

(provide
 for/reducer
 for*/reducer
 (contract-out
  [reducer? predicate/c]
  [make-fold-reducer
   (->* ((-> any/c any/c any/c) any/c)
        (#:name (or/c interned-symbol? #f))
        reducer?)]
  [make-effectful-fold-reducer
   (->* ((-> any/c any/c any/c) (-> any/c) (-> any/c any/c))
        (#:name (or/c interned-symbol? #f))
        reducer?)]
  [make-reducer
   (->* (#:starter (-> variant?)
         #:consumer (-> any/c any/c variant?)
         #:finisher (-> any/c any/c)
         #:early-finisher (-> any/c any/c))
        (#:name (or/c interned-symbol? #f))
        reducer?)]
  [reducer-starter (-> reducer? (-> variant?))]
  [reducer-consumer (-> reducer? (-> any/c any/c variant?))]
  [reducer-finisher (-> reducer? (-> any/c any/c))]
  [reducer-early-finisher (-> reducer? (-> any/c any/c))]
  [reduce (-> reducer? any/c ... any/c)]
  [reduce-all (-> reducer? sequence? any/c)]
  [into-sum reducer?]
  [into-product reducer?]
  [into-count reducer?]
  [into-nth (-> natural? reducer?)]
  [into-index-of (-> any/c reducer?)]
  [into-index-where (-> predicate/c reducer?)]
  [into-max (->* () (comparator? #:key (-> any/c any/c)) reducer?)]
  [into-min (->* () (comparator? #:key (-> any/c any/c)) reducer?)]
  [into-string reducer?]
  [join-into-string
   (->* (immutable-string?)
        (#:before-first immutable-string?
         #:before-last immutable-string?
         #:after-last immutable-string?)
        reducer?)]
  [reducer-map
   (->* (reducer?)
        (#:domain (-> any/c any/c) #:range (-> any/c any/c))
        reducer?)]
  [reducer-filter (-> reducer? predicate/c reducer?)]
  [reducer-limit (-> reducer? natural? reducer?)]))

(require (for-syntax racket/base
                     racket/contract/base
                     syntax/parse)
         racket/bool
         racket/list
         racket/math
         rebellion/base/comparator
         rebellion/base/immutable-string
         rebellion/base/option
         rebellion/base/symbol
         rebellion/base/variant
         rebellion/type/record
         rebellion/type/reference
         rebellion/type/wrapper
         syntax/parse/define)

(begin-for-syntax
  (provide
   (contract-out
    [make-reducer-based-for-comprehensions
     (-> syntax? (values (-> syntax? syntax?) (-> syntax? syntax?)))])))

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define-reference-type reducer (starter consumer finisher early-finisher))

(define (make-fold-reducer consumer init-state #:name [name #f])
  (make-effectful-fold-reducer consumer (λ () init-state) values #:name name))

(define (make-effectful-fold-reducer consumer init-state-maker finisher
                                     #:name [name #f])
  (make-reducer #:starter (λ () (variant #:consume (init-state-maker)))
                #:consumer (λ (s v) (variant #:consume (consumer s v)))
                #:finisher finisher
                #:early-finisher values
                #:name name))

(define into-sum (make-fold-reducer + 0 #:name 'into-sum))
(define into-product (make-fold-reducer * 1 #:name 'into-product))
(define into-count (make-fold-reducer (λ (s _) (add1 s)) 0 #:name 'into-count))

(define (reduce red . vs)
  (define starter (reducer-starter red))
  (define consumer (reducer-consumer red))
  (define finisher (reducer-finisher red))
  (define early-finisher (reducer-early-finisher red))
  (let loop ([tagged-state (starter)] [vs vs])
    (define state (variant-value tagged-state))
    (cond
      [(variant-tagged-as? tagged-state '#:early-finish)
       (early-finisher state)]
      [(empty? vs) (finisher state)]
      [else (loop (consumer state (first vs)) (rest vs))])))

(define (reduce-all red seq)
  (define starter (reducer-starter red))
  (define consumer (reducer-consumer red))
  (define finisher (reducer-finisher red))
  (define early-finisher (reducer-early-finisher red))
  (define-values (first-vs generate-rest) (sequence-generate* seq))
  (let loop ([tagged-state (starter)]
             [sequence-position 0]
             [first-vs first-vs]
             [generate-rest generate-rest])
    (define state (variant-value tagged-state))
    (cond
      [(variant-tagged-as? tagged-state '#:early-finish)
       (early-finisher state)]
      [(false? first-vs)
       (finisher state)]
      [(not (equal? (length first-vs) 1))
       (apply raise-result-arity-error
              'reduce-all 1
              (format "\n  in: sequence elements at position ~a"
                      sequence-position)
              first-vs)]
      [else
       (define-values (next-vs next-generate-rest) (generate-rest))
       (loop (consumer state (first first-vs))
             (add1 sequence-position)
             next-vs
             next-generate-rest)])))

(module+ test
  (test-case "reduce"
    (check-equal? (reduce into-sum 1 2 3 4 5) 15)
    (check-equal? (reduce into-product 2 3 5 7) 210)
    (check-equal? (reduce into-count 'a 'b 'c 'd) 4)
    (check-equal? (reduce into-sum) 0)
    (check-equal? (reduce into-product) 1)
    (check-equal? (reduce into-count) 0))
  (test-case "reduce-all"
    (check-equal? (reduce-all into-sum (in-range 6)) 15)
    (check-equal? (reduce-all into-product
                              (in-hash-values (hash 'a 2 'b 3 'c 5 'd 7)))
                  210)
    (check-equal? (reduce-all into-count (in-string "abcde")) 5)))

(define (reducer-map red #:domain [f values] #:range [g values])
  (define consumer (reducer-consumer red))
  (define finisher (reducer-finisher red))
  (define early-finisher (reducer-early-finisher red))
  (make-reducer #:starter (reducer-starter red)
                #:consumer (λ (s v) (consumer s (f v)))
                #:finisher (λ (s) (g (finisher s)))
                #:early-finisher (λ (s) (g (early-finisher s)))))

(define (reducer-filter red pred)
  (define consumer (reducer-consumer red))
  (define (filtering-consumer s v)
    (if (pred v)
        (consumer s v)
        (variant #:consume s)))
  (make-reducer #:starter (reducer-starter red)
                #:consumer filtering-consumer
                #:finisher (reducer-finisher red)
                #:early-finisher (reducer-early-finisher red)))

(define (reducer-limit red amount)
  (define-record-type limited-state (amount-left original-state))
  (define-wrapper-type original-result)
  (define-wrapper-type limited-result)

  (define original-consumer (reducer-consumer red))
  (define original-starter (reducer-starter red))
  (define original-finisher (reducer-finisher red))
  (define original-early-finisher (reducer-early-finisher red))

  (define (start)
    (define original (original-starter))
    (cond
      [(zero? amount)
       (variant #:early-finish
                (limited-result (variant-value original)))]
      [(variant-tagged-as? original '#:consume)
       (define state
         (limited-state #:amount-left amount
                        #:original-state (variant-value original)))
       (variant #:consume state)]
      [else
       (variant #:early-finsih (original-result (variant-value original)))]))

  (define (consume state element)
    (define next-state
      (original-consumer (limited-state-original-state state) element))
    (define next-amount (sub1 (limited-state-amount-left state)))
    (cond
      [(variant-tagged-as? next-state '#:early-finish)
       (variant #:early-finish (original-result (variant-value next-state)))]
      [(zero? next-amount)
       (variant #:early-finish (limited-result (variant-value next-state)))]
      [else
       (variant #:consume
                (limited-state #:amount-left next-amount
                               #:original-state (variant-value next-state)))]))

  (define (finish state)
    (original-finisher (limited-state-original-state state)))

  (define (early-finish state)
    (if (original-result? state)
        (original-early-finisher (original-result-value state))
        (original-finisher (limited-result-value state))))

  (make-reducer #:starter start
                #:consumer consume
                #:finisher finish
                #:early-finisher early-finish
                #:name 'limited))

(module+ test
  (test-case "reducer-map"
    (define into-sum/string
      (reducer-map into-sum #:domain string->number #:range number->string))
    (check-equal? (reduce into-sum/string "1" "2" "3" "4" "5") "15"))
  (test-case "reducer-filter"
    (define numbers-into-sum (reducer-filter into-sum number?))
    (check-equal? (reduce numbers-into-sum 1 'a 'b 2 3 4 'c 5 'd) 15))
  (test-case "reducer-limit"
    (check-equal? (reduce-all (reducer-limit into-string 3) "hello") "hel")
    (check-equal? (reduce-all (reducer-limit into-string 9) "hello") "hello")
    (check-equal? (reduce-all (reducer-limit into-string 0) "hello") "")
    (check-equal? (reduce-all (reducer-limit (into-nth 2) 4) "hello")
                  (present #\l))
    (check-equal? (reduce-all (reducer-limit (into-nth 2) 1) "hello") absent)
    (check-equal? (reduce-all (reducer-limit (into-nth 2) 4) "he") absent)
    (check-equal? (reduce-all (reducer-limit (into-nth 2) 4) "hel")
                  (present #\l))
    (check-equal? (reduce-all (reducer-limit into-list 5) (in-naturals))
                  (list 0 1 2 3 4))))

(define-simple-macro
  (for/reducer/derived original reducer-expr:expr (for-clause ...)
    body ... tail-expr)
  (let ([reducer reducer-expr])
    (define starter (reducer-starter reducer))
    (define consumer (reducer-consumer reducer))
    (define finisher (reducer-finisher reducer))
    (define early-finisher (reducer-early-finisher reducer))
    (for/fold/derived original
      ([tagged-state (starter)]
       #:result (if (variant-tagged-as? tagged-state '#:consume)
                    (finisher (variant-value tagged-state))
                    (early-finisher (variant-value tagged-state))))
      (for-clause ... #:break (variant-tagged-as? tagged-state '#:early-finish))
      body ...
      (consumer (variant-value tagged-state) tail-expr))))

(define-simple-macro
  (for*/reducer/derived original reducer-expr:expr (for-clause ...)
    body ... tail-expr)
  (let ([reducer reducer-expr])
    (define starter (reducer-starter reducer))
    (define consumer (reducer-consumer reducer))
    (define finisher (reducer-finisher reducer))
    (define early-finisher (reducer-early-finisher reducer))
    (for*/fold/derived original
      ([tagged-state (starter)]
       #:result (if (variant-tagged-as? tagged-state '#:consume)
                    (finisher (variant-value tagged-state))
                    (early-finisher (variant-value tagged-state))))
      (for-clause ... #:break (variant-tagged-as? tagged-state '#:early-finish))
      body ...
      (consumer (variant-value tagged-state) tail-expr))))

(define-simple-macro (for/reducer reducer-expr:expr clauses body ... tail-expr)
  #:with original this-syntax
  (for/reducer/derived original reducer-expr clauses body ... tail-expr))

(define-simple-macro (for*/reducer reducer-expr:expr clauses body ... tail-expr)
  #:with original this-syntax
  (for*/reducer/derived original reducer-expr clauses body ... tail-expr))

(define into-list
  (reducer-map (make-fold-reducer (λ (lst v) (cons v lst)) (list))
               #:range reverse))

(module+ test
  (test-case "for/reducer"
    (check-equal? (for/reducer into-list
                               ([n (in-naturals)]
                                [char (in-string "ab1c23d4ef")])
                    (if (char-alphabetic? char) n 'digit))
                  (list 0 1 'digit 3 'digit 'digit 6 'digit 8 9)))
  (test-case "for*/reducer"
    (check-equal? (for*/reducer into-string
                                ([str (in-list (list "foo1" "bar2" "baz3"))]
                                 [char (in-string str)]
                                 #:when (char-alphabetic? char))
                    char)
                  "foobarbaz")))

(begin-for-syntax
  (define (make-reducer-based-for-comprehensions reducer-expr)
    (define (for-comprehension stx)
      (syntax-parse stx
        [(_ clauses body ... tail-expr)
         #:with original stx
         #:with reducer reducer-expr
         #'(for/reducer/derived original reducer clauses body ... tail-expr)]))
    (define (for*-comprehension stx)
      (syntax-parse stx
        [(_ clauses body ... tail-expr)
         #:with original stx
         #:with reducer reducer-expr
         #'(for*/reducer/derived original reducer clauses body ... tail-expr)]))
    (values for-comprehension for*-comprehension)))

(define into-string (reducer-map into-list #:range list->immutable-string))

(define (join-into-string sep
                          #:before-first [before-first ""]
                          #:before-last [before-last sep]
                          #:after-last [after-last ""])
  (define (join strs)
    (immutable-string-join strs sep
                           #:before-first before-first
                           #:before-last before-last
                           #:after-last after-last))
  (reducer-map into-list #:range join))

(module+ test
  (test-case "into-string"
    (check-equal? (reduce-all into-string "hello world") "hello world"))
  (test-case "join-into-string"
    (check-equal? (reduce (join-into-string ", "
                                            #:before-first "["
                                            #:after-last "]")
                          "foo"
                          "bar"
                          "baz")
                  "[foo, bar, baz]")))

(define (into-nth n)
  (make-reducer
   #:starter (λ () (variant #:consume n))
   #:consumer
   (λ (n v)
     (if (zero? n)
         (variant #:early-finish (present v))
         (variant #:consume (sub1 n))))
   #:finisher (λ (_) absent)
   #:early-finisher values
   #:name 'into-nth))

(define (into-index-of v)
  (into-index-where (λ (elem) (equal? elem v)) #:name 'into-index-of))

(define (into-index-where pred #:name [name 'into-index-where])
  (make-reducer
   #:starter (λ () (variant #:consume 0))
   #:consumer
   (λ (index-counter elem)
     (if (pred elem)
         (variant #:early-finish index-counter)
         (variant #:consume (add1 index-counter))))
   #:finisher (λ (_) absent)
   #:early-finisher (λ (index-counter) (present index-counter))
   #:name name))   

(module+ test
  (test-case "into-nth"
    (check-equal? (reduce-all (into-nth 3) "magic") (present #\i))
    (check-equal? (reduce-all (into-nth 10) "magic") absent)
    (check-equal? (reduce-all (into-nth 0) (in-naturals)) (present 0))
    (check-equal? (reduce-all (into-nth 0) "") absent))
  (test-case "into-index-of"
    (check-equal? (reduce-all (into-index-of #\f) "food") (present 0))
    (check-equal? (reduce-all (into-index-of #\f) "hoof") (present 3))
    (check-equal? (reduce-all (into-index-of #\f) "cat") absent))
  (test-case "into-index-where"
    (check-equal? (reduce-all (into-index-where char-whitespace?) "hello world")
                  (present 5))
    (check-equal? (reduce-all (into-index-where char-numeric?) "goodbye world")
                  absent)))

(define-record-type candidate (element key))

(define (compute-candidate elem key-function)
  (candidate #:element elem #:key (key-function elem)))

(define (into-max [comparator real<=>] #:key [key-function values])
  (make-reducer
   #:starter (λ () (variant #:consume absent))
   #:consumer
   (λ (best-candidate elem)
     (define key (key-function elem))
     (cond
       [(absent? best-candidate)
        (variant #:consume (present (candidate #:element elem #:key key)))]
       [else
        (define best-key
          (candidate-key (present-value best-candidate)))
        (cond
          [(equal? (compare comparator best-key key) lesser)
           (variant #:consume (present (candidate #:element elem #:key key)))]
          [else (variant #:consume best-candidate)])]))
   #:finisher (λ (best) (option-map best candidate-element))
   #:early-finisher values
   #:name 'into-max))

(define (into-min [comparator real<=>] #:key [key-function values])
  (into-max (comparator-reverse comparator) #:key key-function))

(module+ test
  (test-case "into-max"
    (check-equal? (reduce (into-max)) absent)
    (check-equal? (reduce (into-max) 2) (present 2))
    (check-equal? (reduce (into-max) 2 7 5 3) (present 7))
    (test-case "key-function"
      (define max (into-max #:key string-length))
      (check-equal? (reduce max "goodbye" "cruel" "world") (present "goodbye"))
      (check-equal? (reduce max "the" "quick" "brown" "fox") (present "quick")))
    (test-case "custom-comparator"
      (define max (into-max string<=>))
      (check-equal? (reduce max "a" "c" "b") (present "c"))
      (check-equal? (reduce max "abc" "aaa") (present "abc"))))
  (test-case "into-min"
    (check-equal? (reduce (into-min) 4 8 2 16) (present 2))))
