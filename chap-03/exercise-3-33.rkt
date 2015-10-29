#lang racket

(require (combine-in (only-in "../ToolBox/ConstraintSystem/connector.rkt"
                              make-connector
                              set-value!
                              constant
                              probe)
                     (only-in "../ToolBox/ConstraintSystem/constraint.rkt"
                              adder
                              multiplier)))

(define (averager a b avg)
  (let ((sum (make-connector))
        (half (make-connector)))
    (adder a b sum)
    (multiplier sum half avg)
    (constant 0.5 half)
    'OK))

(define a (make-connector))

(define b (make-connector))

(define avg (make-connector))

(averager a b avg)

(probe "arg a" a)
(probe "arg b" b)
(probe "average" avg)

(set-value! a 10 'usr)
(set-value! b 20 'usr)