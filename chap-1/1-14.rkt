#lang racket

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))


(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(count-change 11)

(cc 11 5)

(+ (+ (+ (+ (cc 11 1)
            (+ (+ (cc 6 0)
                  (+ (cc 5 0)
                     (+ (cc 4 0)
                        (+ (cc 3 0)
                           (+ (cc 2 0)
                              (+ (cc 1 0)
                                 (+ (cc 0 0)
                                    (cc -1 1))))))))
               (+ (+ (cc 1 0)
                     (+ (cc 0 0)
                        (cc -1 1)))
                  (cc -4 2))))
         (+ (+ (+ (cc 1 0)
                  (+ (cc 0 0)
                     (cc -1 1)))
               (cc -4 2))
            (cc -9 3)))
      (cc -14 4))
   (cc -39 5))