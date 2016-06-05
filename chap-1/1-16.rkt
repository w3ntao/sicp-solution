#lang racket

(define (even? n)
  (= (remainder n 2)
     0))

(define (square x) (* x x))

(define (iter-fast-expt x n)
  (define (iter base time accumulate)
    (cond ((= time 0)
           accumulate)
          ((even? time)
           (iter (square base)
                 (/ time 2)
                 accumulate))
          (else (iter base
                      (- time 1)
                      (* base accumulate)))))
  (iter x n 1))


(iter-fast-expt 2 0)
(iter-fast-expt 2 1)
(iter-fast-expt 2 2)
(iter-fast-expt 2 3)
(iter-fast-expt 2 4)
(iter-fast-expt 2 5)
(iter-fast-expt 2 6)
(iter-fast-expt 2 7)
(iter-fast-expt 2 8)
(iter-fast-expt 2 9)