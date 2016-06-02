#lang racket

(require (only-in "../ToolBox/CoordinateSystem/vector.rkt"
                  make-vect
                  display-vect))

(define (make-segment v-0 v-1)
  (cons v-0 v-1))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (display-segment segment)
  (display-vect (start-segment segment))
  (display "-")
  (display-vect (end-segment segment))
  (newline))

(define v-0 (make-vect 0 0))

(define v-1 (make-vect 1 1))

(display-segment (make-segment v-0 v-1))