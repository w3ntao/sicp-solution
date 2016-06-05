#lang racket

(require (only-in "../ToolBox/AbstractionOfData/set.rkt"
                  nil
                  element-of-set?))

(define (make-leaf symbol weight)
  (list 'leaf
        symbol
        weight))

(define (leaf? object)
  (eq? (car object)
       'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left)
           (weight right))))


(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (decode raw-bits raw-tree)
  (define (decode-sub bits current-branch)
    (if (null? bits)
        nil
        (let ((next-branch (choose-branch (car bits)
                                          current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-sub (cdr bits)
                                raw-tree))
              (decode-sub (cdr bits)
                          next-branch)))))
  (decode-sub raw-bits raw-tree))

(define (choose-branch bit branch)
  (cond ((= bit 0)
         (left-branch branch))
        ((= bit 1)
         (right-branch branch))
        (else
         (error "bad bit" bit))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(display (decode sample-message sample-tree))