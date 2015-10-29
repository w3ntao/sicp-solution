#lang racket

(require (only-in "../ToolBox/AbstractionOfData/tree.rkt"
                  make-tree
                  entry
                  left-branch
                  right-branch))

(define nil '())

(define (element-of-set? x set)
  (cond ((null? set)
         false)
        ((= x (entry set))
         true)
        ((< x (entry set))
         (element-of-set? x
                          (left-branch set)))
        ((> x (entry set))
         (element-of-set? x
                          (right-branch set)))))

(define (tree-to-list-0 tree)
  (if (null? tree)
      nil
      (append (tree-to-list-0 (left-branch tree))
              (cons (entry tree)
                    (tree-to-list-0 (right-branch tree))))))

(define (tree-to-list-1 raw-tree)
  (define (copy-to-tree tree result-list)
    (if (null? tree)
        result-list
        (copy-to-tree (left-branch tree)
                      (cons (entry tree)
                            (copy-to-tree (right-branch tree)
                                          result-list)))))
  (copy-to-tree raw-tree nil))

(define t-3 (make-tree 3
                       (make-tree 1 nil nil)
                       (make-tree 5 nil nil)))
(define t-9 (make-tree 9
                       (make-tree 8 nil nil)
                       (make-tree 11 nil nil)))
(define test-tree (make-tree 7
                             t-3
                             t-9))

(display (tree-to-list-0 test-tree))