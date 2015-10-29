#lang racket

(define (square-tree tree)
  (tree-map square tree))

(define (square x)
  (* x x))

(define (tree-map f tree)
  (map (lambda (x)
         (if (pair? x)
             (tree-map f x)
             (f x)))
       tree))


(define test-case (list 1
                        (list 2 (list 3 4) 5)
                        (list 6 7)))

(square-tree test-case)