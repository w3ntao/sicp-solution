#lang racket

(require (combine-in (only-in "../ToolBox/ConstraintSystem/constraint.rkt"
                              adder
                              multiplier)
                     (only-in "../ToolBox/ConstraintSystem/connector.rkt"
                              make-connector
                              constant
                              set-value!
                              probe)))

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x)
    z))

(define (cv val)
  (let ((x (make-connector)))
    (constant val x)
    x))


(define C (make-connector))

(define F (celsius-fahrenheit-converter C))

(probe 'C C)
(probe 'F F)


(set-value! C 25 'usr)
