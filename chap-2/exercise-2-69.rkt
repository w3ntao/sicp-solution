#lang racket

(require (only-in "../ToolBox/AbstractionOfData/huffmanTree.rkt"
                  weight
                  make-leaf
                  make-code-tree
                  encode))

(provide (all-defined-out))

(define nil '())

(define (adjoin-set x set)
  (cond ((null? set)
         (list x))
        ((< (weight x)
            (weight (car set)))
         (cons x
               set))
        (else
         (cons (car set)
               (adjoin-set x
                           (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      nil
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (if (= (length leaf-set) 1)
      (car leaf-set)
      (successive-merge (adjoin-set (make-code-tree (car leaf-set)
                                                    (cadr leaf-set))
                                    (cdr (cdr leaf-set))))))

(define pairs (list (list 'A 4) (list 'B 2) (list 'C 1) (list 'D 1)))
(define new-tree (generate-huffman-tree pairs))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(define sample-code '(A D A B B C A))

(display (encode sample-code new-tree))