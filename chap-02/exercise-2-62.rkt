#lang racket

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

(define list-0 (list 1 3 5 7 9))
(define list-1 (list 2 4 6 8 10))

(display (union-set list-0 list-1))