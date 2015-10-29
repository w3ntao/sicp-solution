#lang racket

(require (combine-in (only-in "../ToolBox/AbstractionOfData/list.rkt"
                              flatmap
                              enumerate-interval)
                     (only-in "../ToolBox/Math/prime.rkt"
                              make-pair-sum
                              prime-sum?)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(display (prime-sum-pairs 20))