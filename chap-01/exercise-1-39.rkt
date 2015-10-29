#lang racket

(define (square x)
  (* x x))

(define (get-pi-iter-procedure n temp)
  (if (= n 0)
      temp
      (get-pi-iter-procedure (- n 1)
                             (* temp
                                (/ (square (* 2 n))
                                   (* (- (* 2 n)
                                         1)
                                      (+ (* 2 n)
                                         1)))))))

(define (get-pi-iter n)
  (get-pi-iter-procedure n 2.0))

(define large-number 1000000)

(define get-pi
  (get-pi-iter large-number))


(define (cont-frac n d k)
  (cont-frac-iter n
                  d
                  (- k 1)
                  (- k 1)
                  (/ (n k)
                     (d k))))


(define (cont-frac-iter n d i k temp)
  (if (= i 0)
      temp
      (cont-frac-iter n
                      d
                      (- i 1)
                      k
                      (/ (n i)
                         (- (d i)
                            temp)))))

(define (tan-cf x)
  (define (get-N i)
    (if (= i 1)
        x
        (square x)))
  (define (get-D i)
    (- (* 2 i)
       1))
  (cont-frac get-N get-D large-number))

(tan-cf (/ get-pi 4.0))
get-pi

;(get-pi-iter large-number)

;(/ (n i) (d i))
