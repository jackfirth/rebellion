#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [async-task? predicate/c]
  [running-async-task? predicate/c]
  [successful-async-task? predicate/c]))

(require rebellion/collection/list
         rebellion/type/object
         rebellion/type/singleton
         rebellion/type/tuple
         rebellion/type/wrapper)

;@------------------------------------------------------------------------------

(define-singleton-type not-ready)
(define-tuple-type task-result (value))
(define-tuple-type task-failure (cause))
(define-tuple-type task-cancellation (reason))

(struct exn:fail:async exn:fail (cause) #:transparent)
(struct exn:fail:cancel exn:fail (cause) #:transparent)

(define-object-type async-task
  (result-box
   background-tasks-box
   await-ready-evt
   await-finished-evt
   cancellation-channel))

(define ((make-task-subtype-predicate result-predicate) v)
  (and (async-task? v)
       (result-predicate (unbox (async-task-result-box v)))))

(define running-async-task? (make-task-subtype-predicate not-ready?))
(define successful-async-task? (make-task-subtype-predicate task-result?))
(define failed-async-task? (make-task-subtype-predicate task-failure?))
(define cancelled-async-task? (make-task-subtype-predicate task-cancellation?))

(define (sync-task evt)
  (define result-box (box not-ready))
  (define cancellation-channel (make-channel))
  (define ready-evt
    ;; TODO: figure out how to handle exceptions thrown by evt
    ;; TODO: await cancellation channel and set reason
   (wrap-evt evt (λ (v) (set-box! result-box v))))
  (make-async-task
   #:result-box result-box
   #:background-tasks-box (box empty-list)
   #:await-ready-evt ready-evt
   #:await-finished-evt ready-evt ;; TODO: make this wait for background tasks
   #:cancellation-channel cancellation-channel))

;@------------------------------------------------------------------------------
