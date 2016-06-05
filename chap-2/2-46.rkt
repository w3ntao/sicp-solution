#lang racket

(define (add-vect v-0 v-1)
  (make-vect (+ (xcor-vect v-0)
                (xcor-vect v-1))
             (+ (ycor-vect v-0)
                (ycor-vect v-1))))

(define (sub-vect v-0 v-1)
  (make-vect (- (xcor-vect v-0)
                (xcor-vect v-1))
             (- (ycor-vect v-0)
                (ycor-vect v-1))))


(define (scale-vect factor v)
  (make-vect (* factor
                (xcor-vect v))
             (* factor
                (ycor-vect v))))