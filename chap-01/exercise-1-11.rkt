#lang racket

(define (f n)
 (cond ((< n 3)
        n)
       (else 
        (+ (f (- n 1))
           (* 2
              (f (- n 2)))
           (* 3
              (f (- n 3)))))))

(define (iter-f n)
  (iter-f-procedure 2 1 0 n))

(define (iter-f-procedure a b c count)
  (cond ((= count 0) c)
        ((= count 1) b)
        ((= count 2) a)
        (else
         (iter-f-procedure (+ a
                              (* 2 b)
                              (* 3 c))
                           a
                           b
                           (- count 1)))))

(iter-f 1)
(iter-f 2)
(iter-f 3)
(iter-f 4)
(iter-f 5)
(iter-f 6)


;(f 1)
;(f 2)
;(f 3)
;(f 4)
;(f 5)
;(f 6)
