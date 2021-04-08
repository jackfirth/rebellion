#lang racket/base

(require rebellion/type/singleton/base
         rebellion/type/singleton/descriptor
         rebellion/type/singleton/private/definition-macro)

(provide (all-from-out rebellion/type/singleton/base
                       rebellion/type/singleton/descriptor
                       rebellion/type/singleton/private/definition-macro))
