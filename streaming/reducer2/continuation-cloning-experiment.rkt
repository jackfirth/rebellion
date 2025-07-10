#lang racket


(define nondeterminism-prompt-tag (make-continuation-prompt-tag))


(define-syntax-rule (all-possible-results body ...)
  (call-with-nondeterminism (λ () body ...)))


(define (call-with-nondeterminism proc)
  (call-set-producer-with-nondeterminism
   (λ () (set (proc)))))


(define (call-set-producer-with-nondeterminism set-producer)
  (call-with-continuation-prompt
   set-producer
   nondeterminism-prompt-tag
   (λ (k)
     (set-union
      (call-set-producer-with-nondeterminism (λ () (k #true)))
      (call-set-producer-with-nondeterminism (λ () (k #false)))))))


(define (flip)
  (call-with-composable-continuation
   (λ (k) (abort-current-continuation nondeterminism-prompt-tag k))
   nondeterminism-prompt-tag))


#;(all-possible-results
   (list (if (flip) 'a 'b)
         (if (flip) 'a 'b)))


(define (dynamic-log proc)
  (dynamic-wind
   (λ () (displayln "going in"))
   proc
   (λ () (displayln "coming out"))))


#;(all-possible-results
   (dynamic-log
    (λ ()
      (for/sum ([_ (in-range 3)])
        (if (flip) 1 2)))))


(define (flip-sum n)
  (define total (box 0))
  (for ([_ (in-range n)])
    (set-box! total (+ (if (flip) 1 2) (unbox total))))
  (unbox total))


#;(all-possible-results
   (dynamic-log
    (λ ()
      (flip-sum 3))))


(module+ main
  (all-possible-results
   (define total #f)
   (define stack (list 0))
   (define done? #false)
   (dynamic-wind
    (λ ()
      (set! total (first stack))
      (set! done? #false))
    (λ ()
      (for ([_ (in-range 3)])
        (define next (if (flip) 1 2))
        (set! total (+ next total)))
      (set! done? #true)
      (set! stack (rest stack))
      total)
    (λ ()
      (unless done?
        (set! stack (cons total stack)))))))
