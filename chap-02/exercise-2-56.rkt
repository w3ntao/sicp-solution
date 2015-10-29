#lang racket

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

        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation (base exp) (- (exponent exp) 1)))
          (deriv (base exp)
                 var)))
        (else
         error "unknow expression type -- DERIV" exp)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))


(define (variable? x) (symbol? x))

(define (same-variable? v0 v1)
  (and (variable? v0) (variable? v1) (eq? v0 v1)))



(define (make-sum a0 a1)
  (cond ((and (number? a0) (number? a1)) (+ a0 a1))
        (else (list '+ a0 a1))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))



(define (make-product m0 m1)
  (cond ((or (=number? m0 0) (=number? m1 0)) 0)
        ((=number? m0 1) m1)
        ((=number? m1 1) m0)
        ((and (number? m0) (number? m1)) (* m0 m1))
        (else (list '* m0 m1))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multipicand p) (caddr p))


(define (make-exponentiation x n)
  (cond ((= n 0) 1)
        ((= n 1) x)
        (else (list '** x n))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base s) (cadr s))

(define (exponent s) (caddr s))

;(define ())


(display (deriv '(+ x 3) 'x))

;(display (exponentiation? '(** 2 3)))
(newline)


(display (deriv '(** x 5) 'x))






