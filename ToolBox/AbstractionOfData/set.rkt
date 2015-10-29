#lang racket

(provide (all-defined-out))

(define nil '())

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((eq? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (union-set set-0 set-1)
  (cond ((null? set-0) set-1)
        ((null? set-1) set-0)
        (else
         (let ((x-0 (car set-0))
               (x-1 (car set-1)))
           (cond ((< x-0 x-1)
                  (cons x-0
                        (union-set (cdr set-0) set-1)))
                 ((> x-0 x-1)
                  (cons x-1
                        (union-set set-0 (cdr set-1))))
                 (else (cons x-0
                             (union-set (cdr set-0)
                                        (cdr set-1)))))))))

(define (intersection-set set-0 set-1)
  (if (or (null? set-0) (null? set-1))
      nil
      (cond ((= (car set-0) (car set-1))
             (cons (car set-0)
                   (intersection-set (cdr set-0) (cdr set-1))))
            ((< (car set-0) (car set-1))
             (intersection-set (cdr set-0) set-1))
            ((> (car set-0) (car set-1))
             (intersection-set set-0 (cdr set-1))))))