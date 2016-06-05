#lang racket

(require (only-in "../ToolBox/CoordinateSystem/vector.rkt"
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



(define x-0 (make-vect 2 0))

(define y-0 (make-vect 0 1))

(define basic-point (make-vect 0 0))

(define frame-0 (make-frame basic-point x-0 y-0))

(define test-v (make-vect 1 1))

(display-vect ((frame-coord-map frame-0) test-v))

;(display (origin-frame frame-0))
;(display (car ((frame-coord-map frame-0) test-v)))