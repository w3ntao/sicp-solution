#lang racket

(define (prime? n)
  (= n (smallest-devisor n)))

(define (smallest-devisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n)
         n)
        ((devides? n test-divisor)
         test-divisor)
        (else
         (find-divisor n
                       (new-next test-divisor)))))

(define (old-next n)
  (+ n 1))

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

(define (search-for-primes start end)
  (define (search-for-primes-iter n)
    (time-prime-test n)
    (when (< n end)
      (search-for-primes-iter (+ n 1))))
  (search-for-primes-iter start))

(define (time-prime-test n)
  (newline)
  (display n)
  (newline)
  (start-prime-test n
                    (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (when (prime? n)
      (report-prime (- (current-inexact-milliseconds)
                       start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(search-for-primes 0 1000)
