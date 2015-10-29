#lang racket

(define (even? x)
  (= 0 (remainder x 2)))

(define (odds? x)
  (= 1 (remainder x 2)))

(define (filter f raw-list)
  (cond ((null? raw-list) '())
        ((f (car raw-list)) (cons (car raw-list)
                                  (filter f (cdr raw-list))))
        (else (filter f (cdr raw-list)))))


(define (same-parity first . other)
  (filter (if (even? first) even?
              odds?)
          (cons first other)))
  



(same-parity 1 2 1 2 3 4 5)


