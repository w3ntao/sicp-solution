#lang racket

(require (only-in "../ToolBox/AbstractionOfData/tree.rkt"
                  make-tree
                  left-branch
                  right-branch
                  tree-to-list))

(define nil '())

(define (list-to-tree elements)
  (define ele-length (length elements))
  (define mid (quotient ele-length 2))
  (cond ((null? elements)
         nil)
        ((= ele-length 1)
         (make-tree (list-ref elements
                              0)
                    nil
                    nil))
        (else
         (make-tree (list-ref elements
                              mid)
                    (list-to-tree (partial-list elements
                                                0
                                                (- mid 1)))
                    (list-to-tree (partial-list elements
                                                (+ mid 1)
                                                (- ele-length 1)))))))

(define (partial-list raw-list begin end)
  (cond ((> begin end)
         nil)
        ((= begin end)
         (list (list-ref raw-list
                         begin)))
        (else
         (cons (list-ref raw-list
                         begin)
               (partial-list raw-list
                             (+ begin 1)
                             end)))))




(define test-tree (list-to-tree (list 0 1 2 3 4 5 6 7 89)))

(display (tree-to-list (left-branch test-tree)))
(newline)
(display (tree-to-list (right-branch test-tree)))