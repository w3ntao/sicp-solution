#lang racket

(define nil `())

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (x)
                       (append (list (car s)) x))
                     rest)))))


;(subsets (list 1 2 3))

(subsets (list 1 2 3))

;(map (lambda (x) (append (list 1) (list x))) (cdr (list 1 2 3)))


