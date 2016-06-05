#lang racket


(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set) true))
        ((< x (car set) false))
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define test-list (list 1 4 5 6 7 8))

(display (adjoin-set 7.5 test-list))