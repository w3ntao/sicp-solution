#lang racket

(require (combine-in (only-in "../ToolBox/AbstractionOfData/tree.rkt"
                              tree-to-list
                              list-to-tree)
                     (only-in "../toolBox/AbstractionOfData/set.rkt"
                              union-set
                              intersection-set)))

(define (union-tree tree-0 tree-1)
  (let ((list-0 (tree-to-list tree-0))
        (list-1 (tree-to-list tree-1)))
    (list-to-tree (union-set list-0
                             list-1))))

(define (intersection-tree tree-0 tree-1)
  (let ((list-0 (tree-to-list tree-0))
        (list-1 (tree-to-list tree-1)))
    (list-to-tree (intersection-set list-0
                                    list-1))))

(define odds (list 1 3 5 6 7 11))
(define evens (list 0 2 3 4 6 8))

(define odds-tree (list-to-tree odds))
(define evens-tree (list-to-tree evens))

(display (tree-to-list (intersection-tree odds-tree evens-tree)))