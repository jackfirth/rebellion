#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [mutable-rb-tree-get! (-> mutable-rb-tree? any/c failure-result/c any/c)]
  [mutable-rb-tree-get-entry! (-> mutable-rb-tree? any/c failure-result/c entry?)]
  [mutable-rb-tree-put! (-> mutable-rb-tree? any/c any/c void?)]
  [mutable-rb-tree-put-if-absent! (-> mutable-rb-tree? any/c any/c option?)]
  [mutable-rb-tree-update! (-> mutable-rb-tree? any/c (-> any/c any/c) failure-result/c void?)]))


(require rebellion/base/comparator
         rebellion/base/option
         rebellion/collection/entry
         rebellion/collection/private/mutable-red-black-tree-base
         rebellion/collection/private/mutable-red-black-tree-search
         rebellion/private/guarded-block
         rebellion/private/static-name)


(module+ test
  (require (submod "..")
           racket/sequence
           rebellion/collection/entry
           rebellion/collection/private/mutable-red-black-tree-iteration
           (submod rebellion/collection/private/testing/mutable-red-black-tree-invariants test)
           rackunit))


;@----------------------------------------------------------------------------------------------------


(define/guard (mutable-rb-tree-put! tree key value)
  (define previous-leaf (mutable-rb-tree-get-node tree key))
  (guard (nil-leaf? previous-leaf) else
    (mutable-rb-node-set-value! previous-leaf value))
  (mutable-rb-tree-put-absent! tree previous-leaf key value))


(define/guard (mutable-rb-tree-put-if-absent! tree key value)
  (define previous-leaf (mutable-rb-tree-get-node tree key))
  (guard (nil-leaf? previous-leaf) else
    (present (mutable-rb-node-value previous-leaf)))
  (mutable-rb-tree-put-absent! tree previous-leaf key value)
  absent)


(define/guard (mutable-rb-tree-get! tree key failure-result)
  (define previous-leaf (mutable-rb-tree-get-node tree key))
  (guard (nil-leaf? previous-leaf) else
    (mutable-rb-node-value previous-leaf))
  (define value (if (procedure? failure-result) (failure-result) failure-result))
  (mutable-rb-tree-put-absent! tree previous-leaf key value)
  value)


(define/guard (mutable-rb-tree-get-entry! tree key failure-result)
  (define previous-leaf (mutable-rb-tree-get-node tree key))
  (guard (nil-leaf? previous-leaf) else
    (mutable-rb-node-entry previous-leaf))
  (define value (if (procedure? failure-result) (failure-result) failure-result))
  (mutable-rb-tree-put-absent! tree previous-leaf key value)
  (entry key value))


(define/guard (mutable-rb-tree-update! tree key updater failure-result)
  (define previous-leaf (mutable-rb-tree-get-node tree key))
  (guard (nil-leaf? previous-leaf) else
    (define new-value (updater (mutable-rb-node-value previous-leaf)))
    (mutable-rb-node-set-value! previous-leaf new-value))
  (define value (updater (if (procedure? failure-result) (failure-result) failure-result)))
  (mutable-rb-tree-put-absent! tree previous-leaf key value))


(define/guard (mutable-rb-tree-put-absent! tree previous-leaf key value)
  (guard (root-node? previous-leaf) then
    (mutable-rb-tree-add-root-child! tree (make-red-node key value)))
  (define parent (mutable-rb-node-parent previous-leaf))
  (define direction (mutable-rb-node-parent-direction previous-leaf))
  (define node (make-red-node key value))
  (mutable-rb-node-add-child! parent direction node)
  
  (define/guard (rebalancing-loop node parent)
    
    (guard (red-node? parent) else
      ;; Insertion case 3: parent is black. No rebalancing necessary, black parent node with new red
      ;; child node is fine.
      (void))
    
    (define grandparent (mutable-rb-node-parent parent))
    
    (guard (mutable-rb-root? grandparent) then
      ;; Insertion case 6: parent is red and root.
      (mutable-rb-node-repaint! parent black))
    
    (define grandparent-parent-direction
      (if (equal? (mutable-rb-node-child grandparent left) parent) left right))
    (define grandparent-uncle-direction (direction-inverse grandparent-parent-direction))
    (define uncle
      (mutable-rb-node-child grandparent grandparent-uncle-direction))
    
    (guard (black-node? uncle) then
      ;; Insertion cases 4 and 5: parent is red and uncle is black.
      (cond
        [(equal? (mutable-rb-node-parent-direction node) grandparent-parent-direction)
         ;; Insertion case 5: parent is red, uncle is black, and this node is an "outer child" of the
         ;; parent.
         (mutable-rb-node-rotate! grandparent grandparent-uncle-direction)
         (mutable-rb-node-repaint! parent black)
         (mutable-rb-node-repaint! grandparent red)]
        [else
         ;; Insertion case 4: parent is red, uncle is black, and this node is an "inner child" of the
         ;; parent.
         (mutable-rb-node-rotate! parent grandparent-parent-direction)
         (define new-parent (mutable-rb-node-child grandparent grandparent-parent-direction))
         (mutable-rb-node-rotate! grandparent grandparent-uncle-direction)
         (mutable-rb-node-repaint! new-parent black)
         (mutable-rb-node-repaint! grandparent red)]))
    
    ;; Insertion cases 1 and 2: parent and uncle both red. We repaint them black and repaint the
    ;; grandparent red, then rebalance the grandparent.
    (mutable-rb-node-repaint! parent black)
    (mutable-rb-node-repaint! uncle black)
    (mutable-rb-node-repaint! grandparent red)
    (define great-grandparent (mutable-rb-node-parent grandparent))
    (guard (mutable-rb-root? great-grandparent) then
      ;; Insertion case 2: same as case 1, except the grandparent is the root so after repainting it
      ;; the tree is already fully balanced.
      (void))
    (rebalancing-loop grandparent great-grandparent))
  
  (rebalancing-loop node parent))


(module+ test
  (test-case (name-string mutable-rb-tree-put!)
      
    (test-case "insert one element into empty tree"
      (define tree (make-mutable-rb-tree natural<=>))
      (mutable-rb-tree-put! tree 5 'a)
      (check-mutable-rb-tree-invariants tree)
      (define entries (sequence->list (in-mutable-rb-tree tree)))
      (check-equal? entries (list (entry 5 'a))))
      
    (test-case "insert two ascending elements into empty tree"
      (define tree (make-mutable-rb-tree natural<=>))
      (mutable-rb-tree-put! tree 5 'a)
      (check-mutable-rb-tree-invariants tree)
      (mutable-rb-tree-put! tree 10 'b)
      (check-mutable-rb-tree-invariants tree)
      (define entries (sequence->list (in-mutable-rb-tree tree)))
      (check-equal? entries (list (entry 5 'a) (entry 10 'b))))
    
    (test-case "insert two descending elements into empty tree"
      (define tree (make-mutable-rb-tree natural<=>))
      (mutable-rb-tree-put! tree 5 'a)
      (check-mutable-rb-tree-invariants tree)
      (mutable-rb-tree-put! tree 2 'b)
      (check-mutable-rb-tree-invariants tree)
      (define entries (sequence->list (in-mutable-rb-tree tree)))
      (check-equal? entries (list (entry 2 'b) (entry 5 'a))))
      
    (test-case "insert many ascending elements into empty tree"
      (define tree (make-mutable-rb-tree natural<=>))
      (mutable-rb-tree-put! tree 1 'a)
      (check-mutable-rb-tree-invariants tree)
      (mutable-rb-tree-put! tree 2 'b)
      (check-mutable-rb-tree-invariants tree)
      (mutable-rb-tree-put! tree 3 'c)
      (check-mutable-rb-tree-invariants tree)
      (mutable-rb-tree-put! tree 4 'd)
      (check-mutable-rb-tree-invariants tree)
      (mutable-rb-tree-put! tree 5 'e)
      (check-mutable-rb-tree-invariants tree)
      (define entries (sequence->list (in-mutable-rb-tree tree)))
      (check-equal? entries (list (entry 1 'a) (entry 2 'b) (entry 3 'c) (entry 4 'd) (entry 5 'e))))
      
    (test-case "insert many descending elements into empty tree"
      (define tree (make-mutable-rb-tree natural<=>))
      (mutable-rb-tree-put! tree 5 'a)
      (check-mutable-rb-tree-invariants tree)
      (mutable-rb-tree-put! tree 4 'b)
      (check-mutable-rb-tree-invariants tree)
      (mutable-rb-tree-put! tree 3 'c)
      (check-mutable-rb-tree-invariants tree)
      (mutable-rb-tree-put! tree 2 'd)
      (check-mutable-rb-tree-invariants tree)
      (mutable-rb-tree-put! tree 1 'e)
      (check-mutable-rb-tree-invariants tree)
      (define entries (sequence->list (in-mutable-rb-tree tree)))
      (check-equal? entries (list (entry 1 'e) (entry 2 'd) (entry 3 'c) (entry 4 'b) (entry 5 'a))))
      
    (test-case "insert ascending and descending elements into empty tree"
      (define tree (make-mutable-rb-tree natural<=>))
      (mutable-rb-tree-put! tree 2 'a)
      (check-mutable-rb-tree-invariants tree)
      (mutable-rb-tree-put! tree 3 'b)
      (check-mutable-rb-tree-invariants tree)
      (mutable-rb-tree-put! tree 1 'c)
      (check-mutable-rb-tree-invariants tree)
      (define entries (sequence->list (in-mutable-rb-tree tree)))
      (check-equal? entries (list (entry 1 'c) (entry 2 'a) (entry 3 'b))))
      
    (test-case "insert many ascending and descending elements into empty tree"
      (define tree (make-mutable-rb-tree natural<=>))
      (mutable-rb-tree-put! tree 3 'a)
      (check-mutable-rb-tree-invariants tree)
      (mutable-rb-tree-put! tree 5 'b)
      (check-mutable-rb-tree-invariants tree)
      (mutable-rb-tree-put! tree 1 'c)
      (check-mutable-rb-tree-invariants tree)
      (mutable-rb-tree-put! tree 4 'd)
      (check-mutable-rb-tree-invariants tree)
      (mutable-rb-tree-put! tree 2 'e)
      (check-mutable-rb-tree-invariants tree)
      (define entries (sequence->list (in-mutable-rb-tree tree)))
      (check-equal? entries (list (entry 1 'c) (entry 2 'e) (entry 3 'a) (entry 4 'd) (entry 5 'b))))
      
    (test-case "insert repeatedly ascending then descending elements into empty tree"
      (define tree (make-mutable-rb-tree natural<=>))
      (mutable-rb-tree-put! tree 1 'a)
      (check-mutable-rb-tree-invariants tree)
      (mutable-rb-tree-put! tree 7 'b)
      (check-mutable-rb-tree-invariants tree)
      (mutable-rb-tree-put! tree 2 'c)
      (check-mutable-rb-tree-invariants tree)
      (mutable-rb-tree-put! tree 6 'd)
      (check-mutable-rb-tree-invariants tree)
      (mutable-rb-tree-put! tree 3 'e)
      (check-mutable-rb-tree-invariants tree)
      (mutable-rb-tree-put! tree 5 'f)
      (check-mutable-rb-tree-invariants tree)
      (mutable-rb-tree-put! tree 4 'g)
      (check-mutable-rb-tree-invariants tree)
      (define entries (sequence->list (in-mutable-rb-tree tree)))
      (check-equal?
       entries
       (list
        (entry 1 'a) (entry 2 'c) (entry 3 'e) (entry 4 'g) (entry 5 'f) (entry 6 'd) (entry 7 'b))))
      
    (test-case "insert repeatedly descending then ascending elements into empty tree"
      (define tree (make-mutable-rb-tree natural<=>))
      (mutable-rb-tree-put! tree 7 'a)
      (check-mutable-rb-tree-invariants tree)
      (mutable-rb-tree-put! tree 1 'b)
      (check-mutable-rb-tree-invariants tree)
      (mutable-rb-tree-put! tree 6 'c)
      (check-mutable-rb-tree-invariants tree)
      (mutable-rb-tree-put! tree 2 'd)
      (check-mutable-rb-tree-invariants tree)
      (mutable-rb-tree-put! tree 5 'e)
      (check-mutable-rb-tree-invariants tree)
      (mutable-rb-tree-put! tree 3 'f)
      (check-mutable-rb-tree-invariants tree)
      (mutable-rb-tree-put! tree 4 'g)
      (check-mutable-rb-tree-invariants tree)
      (define entries (sequence->list (in-mutable-rb-tree tree)))
      (check-equal?
       entries
       (list
        (entry 1 'b) (entry 2 'd) (entry 3 'f) (entry 4 'g) (entry 5 'e) (entry 6 'c) (entry 7 'a))))
      
    (test-case "insert many ascending elements then many descending elements into empty tree"
      (define tree (make-mutable-rb-tree natural<=>))
      (mutable-rb-tree-put! tree 4 'a)
      (check-mutable-rb-tree-invariants tree)
      (mutable-rb-tree-put! tree 5 'b)
      (check-mutable-rb-tree-invariants tree)
      (mutable-rb-tree-put! tree 6 'c)
      (check-mutable-rb-tree-invariants tree)
      (mutable-rb-tree-put! tree 7 'd)
      (check-mutable-rb-tree-invariants tree)
      (mutable-rb-tree-put! tree 3 'e)
      (check-mutable-rb-tree-invariants tree)
      (mutable-rb-tree-put! tree 2 'f)
      (check-mutable-rb-tree-invariants tree)
      (mutable-rb-tree-put! tree 1 'g)
      (check-mutable-rb-tree-invariants tree)
      (define entries (sequence->list (in-mutable-rb-tree tree)))
      (check-equal?
       entries
       (list
        (entry 1 'g)
        (entry 2 'f)
        (entry 3 'e)
        (entry 4 'a)
        (entry 5 'b)
        (entry 6 'c)
        (entry 7 'd))))))
