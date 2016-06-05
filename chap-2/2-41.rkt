#lang racket

(require (only-in "../ToolBox/AbstractionOfData/list.rkt"
                  nil
                  flatmap
                  enumerate-interval
                  accumulate))

(define (tri-sum-equal x n)
  (= (+ (list-ref x 0)
        (list-ref x 1)
        (list-ref x 2))
     n))

(define (tri-sum n)
  (filter (lambda (x)
            (tri-sum-equal x n))
          (depack (depack (row-ordered-tri-seq n)))))

(define (row-ordered-tri-seq n)
  (map (lambda (i)
         (map (lambda (j)
                (map (lambda (k) (list i j k))
                     (enumerate-interval (+ j 1)
                                         n)))
              (enumerate-interval (+ i 1)
                                  (/ n 2))))
       (enumerate-interval 1
                           (/ n 3))))

(define (depack raw-list)
  (accumulate append
              nil
              raw-list))

(display (tri-sum 20))
