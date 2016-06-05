#lang racket

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed
              trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

(define (estimate-pi trials circle-x circle-y r x-0 x-1 y-0 y-1)
  (define (experiment)
    (in-circle circle-x circle-y r x-0 x-1 y-0 y-1))
  (/ (* (* (differ x-0 x-1)
           (differ y-0 y-1))
        (monte-carlo trials
                     experiment))
     (square r)))

(define (differ a b)
  (abs (- a b)))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (in-circle circle-x circle-y r x-0 x-1 y-0 y-1)
  (define rand-x
    (if (< x-0 x-1)
        (random-in-range x-0 x-1)
        (random-in-range x-1 x-0)))
  (define rand-y
    (if (< y-0 y-1)
        (random-in-range y-0 y-1)
        (random-in-range y-1 y-0)))
  (< (+ (square (- rand-x circle-x))
        (square (- rand-y circle-y)))
     (square r)))

(define (square x)
  (* x x))

(define large-num 10000)
(define div-num 10000.0)

(define (random-in-range low high)
  (+ low
     (/ (random (* (- high low)
                   large-num))
        div-num)))

(estimate-pi 100000 1.0 1.0 1.0 0 3 0 2)