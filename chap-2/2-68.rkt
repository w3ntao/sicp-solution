#lang racket

(require (combine-in (only-in "../ToolBox/AbstractionOfData/huffmanTree.rkt"
                              leaf?
                              make-leaf
                              left-branch
                              right-branch
                              make-code-tree
                              symbols
                              ;this symbols is different from the system one
                              )
                     (only-in "../ToolBox/AbstractionOfData/set.rkt"
                              nil
                              element-of-set?)))

(define (encode message tree)
  (if (null? message)
      nil
      (append (encode-symbol (car message)
                             tree)
              (encode (cdr message)
                      tree))))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree)
         nil)
        ((element-of-set? symbol
                          (symbols (left-branch tree)))
         (cons '0
               (encode-symbol symbol
                              (left-branch tree))))
        ((element-of-set? symbol
                          (symbols (right-branch tree)))
         (cons '1
               (encode-symbol symbol
                              (right-branch tree))))
        (else (error "error !!"))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define sample-code '(A D A B B C A))

(display (encode sample-code sample-tree))