#lang racket

(define (split op-0 op-1 painter n)
  (if (= n 0)
      painter
      (let ((smaller (split op-0 op-1 painter (- n 1))))
        (op-0 painter (op-1 smaller smaller)))))

