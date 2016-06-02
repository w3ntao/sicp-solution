#lang racket

(define (fib n)
  (let fib-iter ((b 1)
                 (a 0)
                 (count n))
    (if (= count 0)
        a
        (fib-iter b
                  (+ a b)
                  (- count 1)))))

(fib 0)
(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(newline)

(define (new-fib b a n)
  (if (= n 0)
      a
      (new-fib b
               (+ a b)
               (- n 1))))

(new-fib 1 0 0)
(new-fib 1 0 1)
(new-fib 1 0 2)
(new-fib 1 0 3)
(new-fib 1 0 4)