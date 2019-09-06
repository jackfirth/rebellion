#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [signal-output (-> #:state any/c #:on? boolean? signal-output?)]
  [signal-output? predicate/c]
  [signal-output-state (-> signal-output? any/c)]
  [signal-output-on? (-> signal-output? boolean?)]
  [stream-signal? predicate/c]
  [make-fold-stream-signal
   (->* ((-> any/c any/c signal-output?) signal-output?)
        (#:name (or/c interned-symbol? #f))
        stream-signal?)]
  [filtering-on (-> stream-signal? transducer?)]
  [taking-while-on (-> stream-signal? transducer?)]
  [dropping-while-on (-> stream-signal? transducer?)]
  [increasing? (-> (-> any/c any/c (or/c '< '= '>)) stream-signal?)]))

(require racket/contract/region
         rebellion/base/option
         rebellion/base/pair
         rebellion/base/symbol
         rebellion/base/variant
         rebellion/collection/list
         rebellion/streaming/transducer
         rebellion/type/record
         rebellion/type/reference)

;@------------------------------------------------------------------------------

(define/contract (impossible _)
  (-> none/c any/c)
  #f)

(define-record-type signal-output (state on?))

(define-reference-type stream-signal (starter consumer finisher))

(define (make-fold-stream-signal f init-output #:name [name #f])
  (make-stream-signal #:starter (λ () init-output)
                      #:consumer f
                      #:finisher void
                      #:name name))

(define-record-type signalling-state (value on? consumed))

(define (signalling signal)
  (define starter (stream-signal-starter signal))
  (define consumer (stream-signal-consumer signal))
  (define finisher (stream-signal-finisher signal))
  (make-transducer
   #:starter (λ () (variant #:consume (signal-output-state (starter))))
   #:consumer (λ (state v)
               (define output (consumer state v))
               (define next-state
                 (signalling-state #:value (signal-output-state output)
                                   #:on? (signal-output-on? output)
                                   #:consumed v))
               (variant #:emit next-state))
   #:emitter (λ (state)
               (emission (variant #:consume (signalling-state-value state))
                         (pair (signalling-state-consumed state)
                               (signalling-state-on? state))))
   #:half-closer (λ (_) (variant #:finish #f))
   #:half-closed-emitter impossible
   #:finisher void
   #:name 'signalling))

(define (filtering-on signal)
  (transducer-pipe (signalling signal)
                   (filtering pair-second)
                   (mapping pair-first)))

(define (taking-while-on signal)
  (transducer-pipe (signalling signal)
                   (taking-while pair-second)
                   (mapping pair-first)))

(define (dropping-while-on signal)
  (transducer-pipe (signalling signal)
                   (dropping-while pair-second)
                   (mapping pair-first)))

(define (increasing? cmp)
  (make-fold-stream-signal
   (λ (state v)
     (define on?
       (option-case state
                    #:present (λ (previous)
                                (not (equal? (cmp previous v) '>)))
                    #:absent (λ () #t)))
     (signal-output #:state (present v)
                    #:on? on?))
   (signal-output #:state absent #:on? #t)
   #:name 'increasing?))

(define (real<=> x y)
  (cond [(< x y) '<] [(= x y) '=] [else '>]))

(transduce (list 0 1 2 3 4 5 2 0 1 2 7 8 9)
           (signalling (increasing? real<=>))
           #:into into-list)
