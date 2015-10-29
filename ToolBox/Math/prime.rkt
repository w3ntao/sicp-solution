#lang racket

(provide (all-defined-out))

(require (only-in "../AbstractionOfData/list.rkt"
                  flatmap
                  enumerate-interval))

(define (square x)
  (* x x))

(define (prime? n)
  (= n (smallest-devisor n)))

(define (smallest-devisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((devides? n test-divisor) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (next n)
  (cond ((= n 2) 3)
        (else (+ n 2))))

(define (devides? a b)
  (= (remainder a b) 0))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
