#lang racket

(define (specific-e n)
  (exp (+ 1 (/ 1 n))
       n))

(define (exp base n)
  (exp-iter base n 1))

(define (exp-iter base n temp)
  (cond ((= n 0)
         temp)
        ((even? n)
         (square (exp-iter base
                           (/ n 2)
                           temp)))
        (else
         (* base
            (exp-iter base
                      (- n 1)
                      temp)))))

(define (even? n)
  (divide n 2))

(define (square n)
  (* n n))

(define (cont-frac n d k)
  (cont-frac-iter n d k 1))

(define (cont-frac-iter n d k temp)
  (if (= k 0)
      temp
      (cont-frac-iter n
                      d
                      (- k 1)
                      (/ (n k)
                         (+ (d k)
                            temp)))))

(define (divide a b)
  (= (remainder a b)
     0))

(define (get-D i)
  (if (divide (+ i 1)
              3)
      (* (+ i 1)
         (/ 2 3))
      1))

(define (calculate-e n)
  (+ 2
     (cont-frac (lambda (i) 1.0)
                get-D
                n)))

(calculate-e 1000000)

(specific-e 1000000000.0)
