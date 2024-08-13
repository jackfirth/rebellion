#lang racket/base


(require racket/contract/base)


(provide
 for/reducer
 for*/reducer
 (contract-out
  [make-fold-reducer
   (->* ((-> any/c any/c any/c) any/c)
        (#:name (or/c interned-symbol? #f))
        reducer?)]
  [make-effectful-fold-reducer
   (->* ((-> any/c any/c any/c) (-> any/c) (-> any/c any/c))
        (#:name (or/c interned-symbol? #f))
        reducer?)]
  [reduce (-> reducer? any/c ... any/c)]
  [reduce-all (-> reducer? sequence? any/c)]
  [into-sum (reducer/c number? number?)]
  [into-product (reducer/c number? number?)]
  [into-count (reducer/c any/c number?)]
  [into-first (reducer/c any/c option?)]
  [into-last (reducer/c any/c option?)]
  [into-option (reducer/c any/c option?)]
  [into-only-element reducer?]
  [into-nth (-> natural? (reducer/c any/c option?))]
  [into-index-of (-> any/c (reducer/c any/c (option/c natural?)))]
  [into-index-where (-> predicate/c (reducer/c any/c (option/c natural?)))]
  [nonempty-into-first reducer?]
  [nonempty-into-last reducer?]
  [into-any-match? (-> predicate/c (reducer/c any/c boolean?))]
  [into-all-match? (-> predicate/c (reducer/c any/c boolean?))]
  [into-none-match? (-> predicate/c (reducer/c any/c boolean?))]
  [into-for-each (-> (-> any/c void?) (reducer/c any/c void?))]
  [into-max (->* () (comparator? #:key (-> any/c any/c)) (reducer/c any/c option?))]
  [into-min (->* () (comparator? #:key (-> any/c any/c)) (reducer/c any/c option?))]
  [nonempty-into-max (->* () (comparator? #:key (-> any/c any/c)) reducer?)]
  [nonempty-into-min (->* () (comparator? #:key (-> any/c any/c)) reducer?)]
  [into-sorted?
   (->* () (comparator? #:key (-> any/c any/c) #:descending? boolean? #:strict? boolean?)
        (reducer/c any/c boolean?))]
  [into-string (reducer/c char? immutable-string?)]
  [into-line (reducer/c char? immutable-string?)]
  [join-into-string
   (->* (immutable-string?)
        (#:before-first immutable-string?
         #:before-last immutable-string?
         #:after-last immutable-string?)
        (reducer/c immutable-string? immutable-string?))]
  [reducer-map
   (->* (reducer?)
        (#:domain (-> any/c any/c) #:range (-> any/c any/c))
        reducer?)]
  [reducer-filter (-> reducer? predicate/c reducer?)]
  [reducer-limit (-> reducer? natural? reducer?)])
 (all-from-out rebellion/streaming/reducer/private/base)
 (all-from-out rebellion/streaming/reducer/private/zip))


(module+ private-for-rebellion-only
  (provide for/reducer/derived
           for*/reducer/derived))


(require (for-syntax racket/base
                     racket/contract/base
                     rebellion/private/for-body)
         racket/bool
         racket/match
         racket/math
         rebellion/base/comparator
         rebellion/base/immutable-string
         rebellion/base/option
         rebellion/base/option/private/guard
         rebellion/base/symbol
         rebellion/base/variant
         guard
         rebellion/private/static-name
         rebellion/streaming/reducer/private/base
         rebellion/streaming/reducer/private/zip
         rebellion/type/record
         rebellion/type/wrapper
         syntax/parse/define)


(begin-for-syntax
  (provide
   (contract-out
    [make-reducer-based-for-comprehensions
     (-> syntax? (values (-> syntax? syntax?) (-> syntax? syntax?)))])))


(module+ test
  (require (submod "..")
           racket/set
           rackunit))


;@----------------------------------------------------------------------------------------------------
;; Core APIs


(define (make-fold-reducer consumer init-state #:name [name #false])
  (make-effectful-fold-reducer consumer (λ () init-state) values #:name name))

(define (make-effectful-fold-reducer consumer init-state-maker finisher
                                     #:name [name #false])
  (make-reducer #:starter (λ () (variant #:consume (init-state-maker)))
                #:consumer (λ (s v) (variant #:consume (consumer s v)))
                #:finisher finisher
                #:early-finisher values
                #:name name))

(define/name into-sum (make-fold-reducer + 0 #:name enclosing-variable-name))
(define/name into-product (make-fold-reducer * 1 #:name enclosing-variable-name))
(define/name into-count (make-fold-reducer (λ (s _) (add1 s)) 0 #:name enclosing-variable-name))

(define (reduce red . vs)
  (define starter (reducer-starter red))
  (define consumer (reducer-consumer red))
  (define finisher (reducer-finisher red))
  (define early-finisher (reducer-early-finisher red))
  (define/guard (loop [tagged-state (starter)] [vs vs])
    (guard-match (variant #:consume state) tagged-state #:else
      (early-finisher (variant-value tagged-state)))
    (guard-match (cons v next-vs) vs #:else
      (finisher state))
    (loop (consumer state v) next-vs))
  (loop))

(define/name (reduce-all red seq)
  (define starter (reducer-starter red))
  (define consumer (reducer-consumer red))
  (define finisher (reducer-finisher red))
  (define early-finisher (reducer-early-finisher red))
  (define-values (first-vs generate-rest) (sequence-generate* seq))
  (define/guard (loop [tagged-state (starter)]
                      [sequence-position 0]
                      [first-vs first-vs]
                      [generate-rest generate-rest])
    (guard-match (variant #:consume state) tagged-state #:else
      (early-finisher (variant-value tagged-state)))
    (guard first-vs #:else
      (finisher state))
    (guard-match (list v) first-vs #:else
      (apply raise-result-arity-error
             enclosing-function-name 1
             (format "\n  in: sequence elements at position ~a" sequence-position)
             first-vs))
    (define-values (next-vs next-generate-rest) (generate-rest))
    (loop (consumer state v) (add1 sequence-position) next-vs next-generate-rest))
  (loop))

(module+ test
  (test-case (name-string reduce)
    (check-equal? (reduce into-sum 1 2 3 4 5) 15)
    (check-equal? (reduce into-product 2 3 5 7) 210)
    (check-equal? (reduce into-count 'a 'b 'c 'd) 4)
    (check-equal? (reduce into-sum) 0)
    (check-equal? (reduce into-product) 1)
    (check-equal? (reduce into-count) 0))
  (test-case (name-string reduce-all)
    (check-equal? (reduce-all into-sum (in-range 6)) 15)
    (check-equal? (reduce-all into-product
                              (in-hash-values (hash 'a 2 'b 3 'c 5 'd 7)))
                  210)
    (check-equal? (reduce-all into-count (in-string "abcde")) 5)))


;@------------------------------------------------------------------------------
;; Non-core APIs

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
  (define (filtering-consumer s v) (if (pred v) (consumer s v) (variant #:consume s)))
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

  (define/guard (start)
    (define original (original-starter))
    (guard-match (variant #:consume original-state) original #:else
      (variant #:early-finish (original-result (variant-value original))))
    (guard (positive? amount) #:else
      (variant #:early-finish (limited-result (variant-value original))))
    (define state (limited-state #:amount-left amount #:original-state original-state))
    (variant #:consume state))

  (define/guard (consume state element)
    (define next-state (original-consumer (limited-state-original-state state) element))
    (define next-amount (sub1 (limited-state-amount-left state)))
    (guard-match (variant #:consume next-state-value) next-state #:else
      (variant #:early-finish (original-result (variant-value next-state))))
    (guard (positive? next-amount) #:else
      (variant #:early-finish (limited-result next-state-value)))
    (variant #:consume (limited-state #:amount-left next-amount #:original-state next-state-value)))

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
  (test-case (name-string reducer-map)
    (define into-sum/string
      (reducer-map into-sum #:domain string->number #:range number->string))
    (check-equal? (reduce into-sum/string "1" "2" "3" "4" "5") "15"))

  (test-case (name-string reducer-filter)
    (define numbers-into-sum (reducer-filter into-sum number?))
    (check-equal? (reduce numbers-into-sum 1 'a 'b 2 3 4 'c 5 'd) 15))
  
  (test-case (name-string reducer-limit)
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


(define-syntax-parse-rule
  (for/reducer/derived original reducer-expr:expr (for-clause ...) body)
  #:declare body (for-body #'original)
  (let ()
    (define reducer reducer-expr)
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
      body.pre-body ...
      (consumer (variant-value tagged-state) (let () body.post-body ...)))))


(define-syntax-parse-rule
  (for*/reducer/derived original reducer-expr:expr (for-clause ...) body)
  #:declare body (for-body #'original)
  (let ()
    (define reducer reducer-expr)
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
      body.pre-body ...
      (consumer (variant-value tagged-state) (let () body.post-body ...)))))


(define-syntax-parse-rule (for/reducer reducer-expr:expr clauses body ... tail-expr)
  (for/reducer/derived #,this-syntax reducer-expr clauses body ... tail-expr))


(define-syntax-parse-rule (for*/reducer reducer-expr:expr clauses body ... tail-expr)
  (for*/reducer/derived #,this-syntax reducer-expr clauses body ... tail-expr))


(define into-list
  (reducer-map (make-fold-reducer (λ (lst v) (cons v lst)) (list)) #:range reverse))


(module+ test
  (test-case (name-string for/reducer)
    (check-equal? (for/reducer into-list
                               ([n (in-naturals)]
                                [char (in-string "ab1c23d4ef")])
                    (if (char-alphabetic? char) n 'digit))
                  (list 0 1 'digit 3 'digit 'digit 6 'digit 8 9)))
  
  (test-case (name-string for*/reducer)
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
         #:with reducer reducer-expr
         #`(for/reducer/derived #,stx reducer clauses body ... tail-expr)]))
    (define (for*-comprehension stx)
      (syntax-parse stx
        [(_ clauses body ... tail-expr)
         #:with reducer reducer-expr
         #`(for*/reducer/derived #,stx reducer clauses body ... tail-expr)]))
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

(define/name into-line
  (make-reducer
   #:starter (λ () (variant #:consume '()))
   #:consumer
   (λ (chars next-char)
     (if (equal? next-char #\newline)
         (variant #:early-finish chars)
         (variant #:consume (cons next-char chars))))
   #:finisher (λ (chars) (list->immutable-string (reverse chars)))
   #:early-finisher (λ (chars) (list->immutable-string (reverse chars)))
   #:name enclosing-variable-name))

(module+ test
  (test-case (name-string into-string)
    (check-equal? (reduce-all into-string "hello world") "hello world"))
  
  (test-case (name-string join-into-string)
    (check-equal? (reduce (join-into-string ", "
                                            #:before-first "["
                                            #:after-last "]")
                          "foo"
                          "bar"
                          "baz")
                  "[foo, bar, baz]"))
  
  (test-case (name-string into-line)
    (check-equal? (reduce-all into-line "hello \n world") "hello ")))

(define/name (into-nth n)
  (make-reducer
   #:starter (λ () (variant #:consume n))
   #:consumer
   (λ (n v)
     (if (zero? n)
         (variant #:early-finish (present v))
         (variant #:consume (sub1 n))))
   #:finisher (λ (_) absent)
   #:early-finisher values
   #:name enclosing-function-name))

;; TODO(https://github.com/jackfirth/rebellion/issues/187): this reducer should
;;   be named into-first but there's no reducer-rename operation yet.
(define into-first (into-nth 0))

(define/name into-last
  (make-fold-reducer (λ (_ v) (present v)) absent
                     #:name enclosing-variable-name))

(define/name (into-index-of v)
  (into-index-where (λ (elem) (equal? elem v)) #:name enclosing-function-name))

(define/name (into-index-where pred #:name [name enclosing-function-name])
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

(define stateless-consume (variant #:consume #f))
(define stateless-early-finish (variant #:early-finish #f))

(define (stateless-starter) stateless-consume)

(define/name (into-any-match? pred)
  (make-reducer
   #:starter stateless-starter
   #:consumer (λ (_ v) (if (pred v) stateless-early-finish stateless-consume))
   #:finisher (λ (_) #f)
   #:early-finisher (λ (_) #t)
   #:name enclosing-function-name))

(define/name (into-all-match? pred)
  (make-reducer
   #:starter stateless-starter
   #:consumer (λ (_ v) (if (pred v) stateless-consume stateless-early-finish))
   #:finisher (λ (_) #t)
   #:early-finisher (λ (_) #f)
   #:name enclosing-function-name))

(define/name (into-none-match? pred)
  (make-reducer
   #:starter stateless-starter
   #:consumer (λ (_ v) (if (pred v) stateless-early-finish stateless-consume))
   #:finisher (λ (_) #t)
   #:early-finisher (λ (_) #f)
   #:name enclosing-function-name))

(define/name (into-for-each handler)
  (define consume-state (variant #:consume #f))
  (make-reducer
   #:starter (λ () consume-state)
   #:consumer
   (λ (_ element)
     (handler element)
     consume-state)
   #:finisher void
   #:early-finisher void
   #:name enclosing-function-name))

(module+ test
  (test-case (name-string into-nth)
    (check-equal? (reduce-all (into-nth 3) "magic") (present #\i))
    (check-equal? (reduce-all (into-nth 10) "magic") absent)
    (check-equal? (reduce-all (into-nth 0) (in-naturals)) (present 0))
    (check-equal? (reduce-all (into-nth 0) "") absent))
  
  (test-case (name-string into-first)
    (check-equal? (reduce-all into-first "horse") (present #\h))
    (check-equal? (reduce-all into-first "") absent))
  
  (test-case (name-string into-last)
    (check-equal? (reduce-all into-last "horse") (present #\e))
    (check-equal? (reduce-all into-last "") absent))
  
  (test-case (name-string into-index-of)
    (check-equal? (reduce-all (into-index-of #\f) "food") (present 0))
    (check-equal? (reduce-all (into-index-of #\f) "hoof") (present 3))
    (check-equal? (reduce-all (into-index-of #\f) "cat") absent))
  
  (test-case (name-string into-index-where)
    (check-equal? (reduce-all (into-index-where char-whitespace?) "hello world")
                  (present 5))
    (check-equal? (reduce-all (into-index-where char-numeric?) "goodbye world")
                  absent))

  (test-case (name-string into-any-match?)
    (define red (into-any-match? char-whitespace?))
    (check-true (reduce-all red "hello world"))
    (check-false (reduce-all red "hello"))
    (check-false (reduce-all red "")))

  (test-case (name-string into-all-match?)
    (define red (into-all-match? char-alphabetic?))
    (check-false (reduce-all red "hello world"))
    (check-true (reduce-all red "hello"))
    (check-true (reduce-all red "")))
  
  (test-case (name-string into-none-match?)
    (define red (into-none-match? char-whitespace?))
    (check-false (reduce-all red "hello world"))
    (check-true (reduce-all red "hello"))
    (check-true (reduce-all red "")))
  
  (test-case (name-string into-for-each)
    (define st (mutable-set))
    (define add-into-set! (into-for-each (λ (v) (set-add! st v))))
    (check-pred void? (reduce add-into-set! 1 2 3))
    (check-equal? st (mutable-set 1 2 3))))

(define/name nonempty-into-first
  (make-reducer
   #:starter (λ () (variant #:consume #false))
   #:consumer (λ (_ element) (variant #:early-finish element))
   #:finisher (λ (_) (raise-arguments-error enclosing-variable-name "expected at least one element"))
   #:early-finisher values
   #:name enclosing-variable-name))

(define/guard (nonempty-into-last-finish state)
  (guard-present last-element state #:else
    (raise-arguments-error (name nonempty-into-last) "expected at least one element"))
  last-element)

(define/name nonempty-into-last
  (make-reducer
   #:starter (λ () (variant #:consume absent))
   #:consumer (λ (_ element) (variant #:consume (present element)))
   #:finisher nonempty-into-last-finish
   #:early-finisher values
   #:name enclosing-variable-name))

(define (into-option-consume previous element)
  (match previous
    [(present first)
     (raise-arguments-error
      (name into-option)
      "expected at most one element"
      "first element" first
      "second element" element)]
    [_ (variant #:consume (present element))]))

(define/name into-option
  (make-reducer
   #:starter (λ () (variant #:consume absent))
   #:consumer into-option-consume
   #:finisher values
   #:early-finisher values
   #:name enclosing-variable-name))

(define (into-only-element-consume previous element)
  (match previous
    [(present first)
     (raise-arguments-error
      (name into-only-element)
      "expected exactly one element, but multiple elements were received"
      "first element" first
      "second element" element)]
    [_ (variant #:consume (present element))]))

(define/guard (into-only-element-finish result-option)
  (guard-present result result-option #:else
    (raise-arguments-error
     (name into-only-element)
     "expected exactly one element, but zero elements were received"))
  result)

(define/name into-only-element
  (make-reducer
   #:starter (λ () (variant #:consume absent))
   #:consumer into-only-element-consume
   #:finisher into-only-element-finish
   #:early-finisher values
   #:name enclosing-variable-name))

(module+ test
  (test-case (name-string nonempty-into-first)
    (check-equal? (reduce nonempty-into-first 1) 1)
    (check-equal? (reduce nonempty-into-first 1 2 3) 1)
    (check-exn exn:fail:contract? (λ () (reduce nonempty-into-first)))
    (check-exn #rx"nonempty-into-first:" (λ () (reduce nonempty-into-first)))
    (check-exn #rx"expected at least one element" (λ () (reduce nonempty-into-first))))

  (test-case (name-string nonempty-into-last)
    (check-equal? (reduce nonempty-into-last 1) 1)
    (check-equal? (reduce nonempty-into-last 1 2 3) 3)
    (check-exn exn:fail:contract? (λ () (reduce nonempty-into-last)))
    (check-exn #rx"nonempty-into-last:" (λ () (reduce nonempty-into-last)))
    (check-exn #rx"expected at least one element" (λ () (reduce nonempty-into-last))))

  (test-case (name-string into-option)
    (check-equal? (reduce into-option) absent)
    (check-equal? (reduce into-option 4) (present 4))
    (check-exn exn:fail:contract? (λ () (reduce into-option 1 2)))
    (check-exn #rx"into-option:" (λ () (reduce into-option 1 2)))
    (check-exn #rx"expected at most one element" (λ () (reduce into-option 1 2)))
    (check-exn #rx"first element: 1" (λ () (reduce into-option 1 2)))
    (check-exn #rx"second element: 2" (λ () (reduce into-option 1 2))))

  (test-case (name-string into-only-element)
    (check-equal? (reduce into-only-element 4) 4)
    (check-exn exn:fail:contract? (λ () (reduce into-only-element 1 2)))
    (check-exn #rx"into-only-element:" (λ () (reduce into-only-element 1 2)))
    (check-exn
     #rx"expected exactly one element, but multiple elements were received"
     (λ () (reduce into-only-element 1 2)))
    (check-exn #rx"first element: 1" (λ () (reduce into-only-element 1 2)))
    (check-exn #rx"second element: 2" (λ () (reduce into-only-element 1 2)))
    (check-exn exn:fail:contract? (λ () (reduce into-only-element)))
    (check-exn #rx"into-only-element:" (λ () (reduce into-only-element)))
    (check-exn
     #rx"expected exactly one element, but zero elements were received"
     (λ () (reduce into-only-element)))))

(define-record-type candidate (element key))

(define (compute-candidate elem key-function)
  (candidate #:element elem #:key (key-function elem)))

(define/name (into-max [comparator real<=>] #:key [key-function values])
  (make-reducer
   #:starter (λ () (variant #:consume absent))
   #:consumer
   (λ (best-candidate elem)
     (guarded-block
      (define key (key-function elem))
      (guard (present? best-candidate) #:else
        (variant #:consume (present (candidate #:element elem #:key key))))
      (define best-key (candidate-key (present-value best-candidate)))
      (guard (equal? (compare comparator best-key key) lesser) #:else
        (variant #:consume best-candidate))
      (variant #:consume (present (candidate #:element elem #:key key)))))
   #:finisher (λ (best) (option-map best candidate-element))
   #:early-finisher values
   #:name enclosing-function-name))

(define (into-min [comparator real<=>] #:key [key-function values])
  (into-max (comparator-reverse comparator) #:key key-function))

(define/guard (check-max result-option)
  (guard-present result result-option #:else
    (raise-arguments-error (name nonempty-into-max) "expected at least one element"))
  result)

(define/guard (check-min result-option)
  (guard-present result result-option #:else
    (raise-arguments-error (name nonempty-into-min) "expected at least one element"))
  result)

(define (nonempty-into-max [comparator real<=>] #:key [key-function values])
  (reducer-map (into-max comparator #:key key-function) #:range check-max))

(define (nonempty-into-min [comparator real<=>] #:key [key-function values])
  (reducer-map (into-min comparator #:key key-function) #:range check-min))

(module+ test
  (test-case (name-string into-max)
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
  
  (test-case (name-string into-min)
    (check-equal? (reduce (into-min) 4 8 2 16) (present 2)))

  (test-case (name-string nonempty-into-max)
    (check-equal? (reduce (nonempty-into-max) 4) 4)
    (check-equal? (reduce (nonempty-into-max) 4 7 3 5) 7)
    (check-exn exn:fail:contract? (λ () (reduce (nonempty-into-max))))
    (check-exn #rx"nonempty-into-max:" (λ () (reduce (nonempty-into-max))))
    (check-exn #rx"expected at least one element" (λ () (reduce (nonempty-into-max)))))

  (test-case (name-string nonempty-into-min)
    (check-equal? (reduce (nonempty-into-min) 4) 4)
    (check-equal? (reduce (nonempty-into-min) 4 7 3 5) 3)
    (check-exn exn:fail:contract? (λ () (reduce (nonempty-into-min))))
    (check-exn #rx"nonempty-into-min:" (λ () (reduce (nonempty-into-min))))
    (check-exn #rx"expected at least one element" (λ () (reduce (nonempty-into-min))))))

(define/name (into-sorted? [comparator real<=>]
                           #:key [key-function values]
                           #:descending? [descending? #false]
                           #:strict? [strict? #false])
  (define expected-result?
    (cond
      [(and descending? strict?) (λ (result) (equal? result greater))]
      [descending? (λ (result) (not (equal? result lesser)))]
      [strict? (λ (result) (equal? result lesser))]
      [else (λ (result) (not (equal? result greater)))]))
  (make-reducer
   #:starter (λ () (variant #:consume absent))
   #:consumer
   (λ (previous-key element)
     (define next-key (key-function element))
     (match previous-key
       [(== absent) (variant #:consume (present next-key))]
       [(present previous)
        (if (expected-result? (compare comparator previous next-key))
            (variant #:consume (present next-key))
            (variant #:early-finish #false))]))
   #:early-finisher (λ (_) #false)
   #:finisher (λ (_) #true)
   #:name enclosing-function-name))

(module+ test
  (test-case (name-string into-sorted?)
    (test-case "defaults"
      (check-true (reduce (into-sorted?) 1 2 3 4 5))
      (check-true (reduce (into-sorted?) 1 2 3 3 4 4 5))
      (check-false (reduce (into-sorted?) 1 2 5 4 3))
      (check-true (reduce (into-sorted?) 1))
      (check-true (reduce (into-sorted?))))

    (test-case "strictly ascending"
      (define reducer (into-sorted? #:strict? #true))
      (check-true (reduce reducer 1 2 3 4 5))
      (check-false (reduce reducer 1 2 3 3 4 4 5))
      (check-false (reduce reducer 1 2 5 4 3))
      (check-true (reduce reducer 1))
      (check-true (reduce reducer)))

    (test-case "descending"
      (define reducer (into-sorted? #:descending? #true))
      (check-true (reduce reducer 5 4 3 2 1))
      (check-true (reduce reducer 5 4 3 3 2 2 1))
      (check-false (reduce reducer 5 4 1 2 3))
      (check-true (reduce reducer 1))
      (check-true (reduce reducer)))

    (test-case "strictly descending"
      (define reducer (into-sorted? #:descending? #true #:strict? #true))
      (check-true (reduce reducer 5 4 3 2 1))
      (check-false (reduce reducer 5 4 3 3 2 2 1))
      (check-false (reduce reducer 5 4 1 2 3))
      (check-true (reduce reducer 1))
      (check-true (reduce reducer)))))
