#lang racket

(require (combine-in (only-in "../ToolBox/AbstractionOfData/table.rkt"
                              nil
                              make-table
                              insert!
                              lookup)))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (if (lookup x table)
          (lookup x table)
          (let ((result (f x)))
            (insert! x
                     result
                     table)
            result)))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0)
                    0)
                   ((= n 1)
                    1)
                   (else
                    (+ (memo-fib (- n 1))
                       (memo-fib (- n 2))))))))

(define (fib n)
  (cond ((= n 0)
         0)
        ((= n 1)
         1)
        (else
         (+ (fib (- n 1))
            (fib (- n 2))))))

(memo-fib 50)