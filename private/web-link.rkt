#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [web-link (-> url? (or/c url? symbol?) url? web-link?)]
  [web-link? (-> any/c boolean?)]
  [web-link-source (-> web-link? url?)]
  [web-link-relation (-> web-link? (or/c url? symbol?))]
  [web-link-target (-> web-link? url?)]))

(require net/url
         rebellion/tuple-type)

(define web-link-descriptor
  (tuple-type-make-implementation (tuple-type 'web-link 3)))

(define web-link (tuple-descriptor-constructor web-link-descriptor))
(define web-link? (tuple-descriptor-predicate web-link-descriptor))

(define web-link-source
  (make-tuple-field-accessor web-link-descriptor 0 'source))

(define web-link-relation
  (make-tuple-field-accessor web-link-descriptor 1 'relation))

(define web-link-target
  (make-tuple-field-accessor web-link-descriptor 2 'target))
