#lang racket

(define (expmod base exp m)
  (cond ((= exp 0)
         1)
        ((even? exp)
         (remainder (square (expmod base
                                    (/ exp 2)
                                    m))
                    m))
        (else
         (remainder (* base
                       (expmod base
                               (- exp 1)
                               m))
                    m))))

(define (even? n)
  (= (remainder n 2)
     0))

(define (fermat-test n)
  (fermat-test-iter n 2))

(define (fermat-prime n num)
  (= (expmod num n n)
     num))

(define (fermat-test-iter n num)
  (cond ((= num n)
         #t)
        ((not (fermat-prime n num))
         #f)
        (else
         (fermat-test-iter n
                           (+ num 1)))))

(define (prime? n)
  (= n
     (smallest-devisor n)))

(define (smallest-devisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor)
            n)
         n)
        ((devides? n
                   test-divisor)
         test-divisor)
        (else
         (find-divisor n
                       (new-next test-divisor)))))

(define (new-next n)
  (cond ((= n 2)
         3)
        (else
         (+ n 2))))

(define (devides? a b)
  (= (remainder a b)
     0))

(define (square n)
  (* n n))

(fermat-test 6601)
(prime? 8)

;(expmod 2 561 100000000000)
;(remainder 98790653952 561)