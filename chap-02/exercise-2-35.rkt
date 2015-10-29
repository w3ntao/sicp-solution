#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (tree)
                     (if (pair? tree)
                         (count-leaves tree)
                         1))
                   t)))

(define test-case (list (list 1 2) (list 3 4 7 8)))

(count-leaves test-case)