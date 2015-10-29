#lang racket

(provide (all-defined-out))

(define nil '())

(define (list-ref items n)
  (if (= n 0) (car items)
      (list-ref (cdr items)
                (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1
         (length (cdr items)))))

(define (sub-list raw-list n)
  (if (= n 0)
      raw-list
      (sub-list (cdr raw-list)
                (- n 1))))

(define (for-each op raw-list)
  (when (not (null? raw-list))
    (op (car raw-list))
    (for-each op (cdr raw-list))))

(define (reverse row-list)
  (define (reverse-iter old-list new-list n)
    (if (= n 0) new-list
        (reverse-iter old-list
                      (append new-list
                              (list (list-ref old-list
                                              (- n 1))))
                      (- n 1))))
  (reverse-iter row-list nil (length row-list)))


(define (fringe x)
  (cond ((null? x) nil)
        ((not (pair? x)) (list x))
        (else (append (fringe (car x))
                      (fringe (cdr x))))))

(define (filter f raw-list)
  (cond ((null? raw-list)
         nil)
        ((f (car raw-list))
         (cons (car raw-list)
               (filter f
                       (cdr raw-list))))
        (else
         (filter f
                 (cdr raw-list)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low
            (enumerate-interval (+ low 1)
                                high))))

(define (accumulate op initial raw-sequence)
  (if (null? raw-sequence)
      initial
      (op (car raw-sequence)
          (accumulate op initial (cdr raw-sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))