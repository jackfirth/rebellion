#lang racket/base

(require rebellion/type/tuple/base
         rebellion/type/tuple/descriptor
         rebellion/type/tuple/private/definition-macro)

(provide (all-from-out rebellion/type/tuple/base
                       rebellion/type/tuple/descriptor
                       rebellion/type/tuple/private/definition-macro))
