#lang racket

(define (count time)
  (lambda (x)
    (begin (set! time
                 (+ time 1))
           (if (= x 1)
               (if (= time 1)
                   1
                   (begin (set! time 0)
                          0))
               0))))

(define f (count 0))

(+ (f 0) (f 1))
(+ (f 1) (f 0))