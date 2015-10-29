#lang racket

(define test-num 5)

((lambda (n)
   (define (fact-lambda fact)
     (fact fact n))
   (define (ft-lambda ft k)
     (if (= k 1)
         1
         (* k
            (ft ft (- k 1)))))
   (fact-lambda ft-lambda))
 test-num)

((lambda (n)
   ((lambda (fibonacci)
      (fibonacci fibonacci n))
    (lambda (fib k)
      (if (< k 2)
          k
          (+ (fib fib (- k 1))
             (fib fib (- k 2)))))))
 test-num)

(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0)
         true
         (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0)
         false
         (ev? ev? od? (- n 1))))))

(f test-num)
