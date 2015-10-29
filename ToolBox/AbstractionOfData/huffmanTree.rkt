#lang racket

(provide (all-defined-out))

(require (combine-in (only-in "set.rkt"
                              nil
                              element-of-set?)))

;left-branch and right-branch are different from the general tree
(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

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
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit" bit))))

(define (encode message tree)
  (if (null? message)
      nil
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) nil)
        ((element-of-set? symbol (symbols (left-branch tree)))
         (cons '0
               (encode-symbol symbol
                              (left-branch tree))))
        ((element-of-set? symbol (symbols (right-branch tree)))
         (cons '1
               (encode-symbol symbol
                              (right-branch tree))))
        (else (error "error!! wrong element -- ENCODE-SYMBOL"))))