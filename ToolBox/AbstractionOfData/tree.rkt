#lang racket

(provide (all-defined-out))

(require (only-in "list.rkt"
                  nil
                  length
                  ;length: different from the system one
                  list-ref))

(define (make-tree entry left right)
  (list entry left right))

(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (tree-to-list raw-tree)
  (define (copy-to-tree tree result-list)
    (if (null? tree)
        result-list
        (copy-to-tree (left-branch tree)
                      (cons (entry tree)
                            (copy-to-tree (right-branch tree)
                                          result-list)))))
  (copy-to-tree raw-tree nil))

(define (partial-list raw-list begin end)
  (cond ((> begin end) nil)
        ((= begin end) (list (list-ref raw-list begin)))
        (else (cons (list-ref raw-list begin)
                    (partial-list raw-list
                                  (+ begin 1)
                                  end)))))

(define (list-to-tree elements)
  (define ele-length (length elements))
  (define mid (quotient ele-length 2))
  (cond ((null? elements) nil)
        ((= ele-length 1) (make-tree (list-ref elements 0)
                                     nil
                                     nil))
        (else (make-tree (list-ref elements mid)
                         (list-to-tree (partial-list elements
                                                     0
                                                     (- mid 1)))
                         (list-to-tree (partial-list elements
                                                     (+ mid 1)
                                                     (- ele-length 1)))))))