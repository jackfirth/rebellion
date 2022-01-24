#lang racket/base


(provide mutable-rbtree!)


(require (for-syntax racket/base)
         rebellion/collection/private/mutable-red-black-tree-base
         syntax/parse/define)


(begin-for-syntax

  (define-syntax-class literal-mutable-rb-node
    #:datum-literals (NIL)
    #:attributes (initialization-expr)

    (pattern (color key:expr (~optional value:expr))
      #:declare color (expr/c #'color?)
      #:with initialization-expr
      #'(make-rb-node color key (~? value #false)))

    (pattern
        (color key:expr (~optional value:expr)
                             (~or NIL left-child:literal-mutable-rb-node)
                             (~or NIL right-child:literal-mutable-rb-node))
      #:declare color (expr/c #'color?)
      #:with initialization-expr
      #'(let ([node (make-rb-node color key (~? value #false))])
          (~? (mutable-rb-node-add-child! node left left-child.initialization-expr))
          (~? (mutable-rb-node-add-child! node right right-child.initialization-expr))
          node))))


(define-simple-macro (mutable-rbtree!
                      #:key-comparator key-comparator:expr root-node:literal-mutable-rb-node)
  (let ([tree (make-mutable-rb-tree key-comparator)]
        [root root-node.initialization-expr])
    (mutable-rb-tree-add-root-child! tree root)
    tree))
