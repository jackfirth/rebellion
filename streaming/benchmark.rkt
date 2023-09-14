#lang racket/base


(require (for-syntax racket/base
                     racket/sequence
                     racket/syntax)
         racket/format
         racket/list
         racket/sequence
         racket/set
         racket/vector
         rebellion/collection/list
         rebellion/collection/set
         rebellion/collection/vector
         (submod rebellion/streaming/reducer no-contract)
         (submod rebellion/streaming/transducer no-contract)
         syntax/parse/define)


(define-syntax-parse-rule
  (benchmark-suite! name (~seq #:param param:id param-expr:expr) ... body ...+)
  (define (name)
    (printf "=== ~a ===\n\n" 'name)
    (begin
      (define param param-expr)
      (printf "~a: ~a\n" 'param param))
    ...
    (newline)
    (newline)
    body ...))


(define-syntax-parse-rule (benchmark! name expr ...+)
  (begin
    (printf "--- ~a ---\n\n" 'name)
    (begin
      (displayln 'expr)
      (benchmark-time-run (λ () expr))
      (newline))
    ...
    (newline)))


(define (benchmark-time-run thunk)
  (define iterations 10)
  (aggressively-collect-garbage)
  (define-values (total-cpu-time total-real-time total-gc-time)
    (for/fold ([total-cpu-time 0] [total-real-time 0] [total-gc-time 0])
              ([_ (in-range 0 iterations)])
      (define-values (unused-result cpu-time real-time gc-time) (time-apply thunk '()))
      (values (+ total-cpu-time cpu-time)
              (+ total-real-time real-time)
              (+ total-gc-time gc-time))))

  (define (average-time x)
    (/ x iterations))
  
  (define average-cpu-time (average-time total-cpu-time))
  (define average-real-time (average-time total-real-time))
  (define average-gc-time (average-time total-gc-time))
  (printf "cpu time: ~a ms real time: ~a ms gc time: ~a ms\n"
          (~r average-cpu-time) (~r average-real-time) (~r average-gc-time)))


(define (aggressively-collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (collect-garbage))


(benchmark-suite! stream-pipeline-benchmarks
  #:param element-count 100000

  (define element-list (range 1 element-count))
  (define element-vector (list->vector element-list))

  (benchmark! into-list
    (vector->list element-vector)
    (sequence->list element-list)
    (for/list ([x element-list]) x)
    (for/list ([x (in-list element-list)]) x)
    (for/list ([x element-vector]) x)
    (for/list ([x (in-vector element-vector)]) x)
    (transduce element-list #:into into-list)
    (transduce element-vector #:into into-list))

  (benchmark! into-vector
    (list->vector element-list)
    (for/vector ([x element-list]) x)
    (for/vector ([x (in-list element-list)]) x)
    (for/vector ([x element-vector]) x)
    (for/vector ([x (in-vector element-vector)]) x)
    (transduce element-list #:into (into-vector))
    (transduce element-vector #:into (into-vector)))

  (benchmark! into-set
    (list->set element-list)
    (for/set ([x element-list]) x)
    (for/set ([x (in-list element-list)]) x)
    (for/set ([x element-vector]) x)
    (for/set ([x (in-vector element-vector)]) x)
    (transduce element-list #:into into-set)
    (transduce element-vector #:into into-set))

  (benchmark! into-sum
    (apply + element-list)
    (apply + (vector->list element-vector))
    (for/sum ([x element-list]) x)
    (for/sum ([x (in-list element-list)]) x)
    (for/sum ([x element-vector]) x)
    (for/sum ([x (in-vector element-vector)]) x)
    (transduce element-list #:into into-sum)
    (transduce element-vector #:into into-sum))

  (benchmark! into-count
    (length element-list)
    (vector-length element-vector)
    (transduce element-list #:into into-count)
    (transduce element-vector #:into into-count)))


(module+ main
  (stream-pipeline-benchmarks))
