#lang racket

(require (only-in "../ToolBox/AbstractionOfData/list.rkt"
                  sub-list))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ;exponentiation? not implemented yet
        #|
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation (base exp) (- (exponent exp) 1)))
          (deriv (base exp)
                 var)))
        |#
        (else
         error "unknow expression type -- DERIV" exp)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))


(define (variable? x) (symbol? x))

(define (same-variable? v0 v1)
  (and (variable? v0) (variable? v1) (eq? v0 v1)))



(define (make-sum a0 a1)
  (cond ((and (number? a0) (number? a1)) (+ a0 a1))
        ((and (number? a0) (= a0 0)) a1)
        ((and (number? a1) (= a1 0)) a0)
        (else (list '+ a0 a1))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (list-ref s 1))

(define (augend s)
  (if (sum? s)
      (make-sum (list-ref s 2)
                (augend (sub-list s 3)))
      (if (null? s)
          0
          (make-sum (car s) (augend (cdr s))))))

(define (make-product m0 m1)
  (cond ((or (=number? m0 0) (=number? m1 0)) 0)
        ((=number? m0 1) m1)
        ((=number? m1 1) m0)
        ((and (number? m0) (number? m1)) (* m0 m1))
        (else (list '* m0 m1))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (list-ref p 1))

(define (multiplicand p)
  (if (product? p)
      (make-product (list-ref p 2)
                (multiplicand (sub-list p 3)))
      (if (null? p)
          1
          (make-product (car p) (multiplicand (cdr p))))))



(define test-exp '(+ x y (+ x 3)))

(define test-exp-2 '(* x y (+ x 3)))


(newline)

(display (deriv test-exp-2 'x))
