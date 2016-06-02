#lang racket

(define nil '())

(define (square x)
  (* x x))


(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

(define (square-list-improve items)
  (define (iter-improve things answer)
    (if (null? things)
        answer
        (iter-improve (cdr things)
              (append answer
                      (list (square (car things)))))))
  (iter-improve items nil))


(square-list (list 1 2 3 5))

(square-list-improve (list 1 2 3 5))


(cons 5 (list 1 2 3 4))

(cons (list 1 2 3 4) 5)

(append (list 1 2 3 4) (list 5))
