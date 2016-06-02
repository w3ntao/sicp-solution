#lang racket


(define (cont-frac n d k)
  (cont-frac-iter n d k 1))

(define (cont-frac-recurse n d i k)
  (if (= i k)
      (/ (n k)
         (d k))
      (/ (n i)
         (+ (d i)
            (cont-frac-recurse n
                               d
                               (+ i 1)
                               k)))))


(define (cont-frac-iter n d k temp)
  (if (= k 0)
      temp
      (cont-frac-iter n d
                      (- k 1)
                      (/ (n k)
                         (+ (d k)
                            temp)))))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10)

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           20)
