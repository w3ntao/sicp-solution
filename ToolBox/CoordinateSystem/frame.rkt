#lang racket

(provide (all-defined-out))

(require (only-in "vector.rkt"
                  make-vect
                  xcor-vect
                  ycor-vect
                  add-vect
                  scale-vect
                  display-vect))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (edge-1-frame frame)
  (car (cdr frame)))

(define (edge-2-frame frame)
  (cdr (cdr frame)))

(define (origin-frame frame)
  (car frame))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge-1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge-2-frame frame))))))


(define (display-frame frame)
  (display "origin: ")
  (display-vect (origin-frame frame))
  (newline)
  (display "edge-1: ")
  (display-vect (edge-1-frame frame))
  (newline)
  (display "edge-2: ")
  (display-vect (edge-2-frame frame))
  (newline))
