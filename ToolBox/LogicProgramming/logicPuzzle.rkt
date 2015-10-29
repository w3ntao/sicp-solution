#lang racket

(require (only-in "../AbstractionOfData/list.rkt"
                  nil
                  flatmap
                  list-ref))

(provide (all-defined-out))

(define (apply-constrains combination constrains)
  (if (null? constrains)
      combination
      (apply-constrains (filter (car constrains)
                                combination)
                        (cdr constrains))))

(define (all-combination raw-list)
  (if (null? raw-list)
      (list nil)
      (flatmap (lambda (x)
                 (insert-item-at-every-position x
                                                (car raw-list)))
               (all-combination (cdr raw-list)))))

(define (insert-item-at-every-position raw-list item)
  (define (insert-item n)
    (append (list (insert-item-at raw-list
                                  n))
            (if (= n 0)
                nil
                (insert-item (- n 1)))))
  (define (insert-item-at temp-list n)
    (if (= n 0)
        (cons item
              temp-list)
        (cons (car temp-list)
              (insert-item-at (cdr temp-list)
                              (- n 1)))))
  (insert-item (length raw-list)))