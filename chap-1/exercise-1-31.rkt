#lang racket

(define (sum term a next b)
  (define (iter a temp)
    (if (> a b) temp
        (iter (next a)
              (+ (term a) temp))))
  (iter a 0))

(define (get-pi-recurse n)
  (if (= n 0) 2.0
      (* (/ (square (* 2 n))
            (* (- (* 2 n) 1)
               (+ (* 2 n) 1)))
         (get-pi-recurse (- n 1)))))

(define (get-pi-iter n)
  (get-pi-iter-procedure n 2.0))

(define (get-pi-iter-procedure n temp)
  (if (= n 0) temp
      (get-pi-iter-procedure (- n 1) (* temp
                                        (/
                                         (square (* 2 n))
                                         (* (- (* 2 n) 1)
                                            (+ (* 2 n) 1)))))))

(define (square x)
  (* x x))

(get-pi-recurse 100)
(get-pi-iter 100)
