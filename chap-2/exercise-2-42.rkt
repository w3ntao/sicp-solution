#lang racket

(require (only-in "../ToolBox/AbstractionOfData/list.rkt"
                  nil
                  flatmap
                  enumerate-interval))

(define (queens board-size)
  (define empty-board nil)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions) (safe? positions))
                (flatmap (lambda (rest-of-queens)
                           (map (lambda (new-row)
                                  (adjoin-position new-row
                                                   rest-of-queens))
                                (enumerate-interval 1 board-size)))
                         (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (adjoin-position new-row rest-of-queens)
  (append rest-of-queens
          (list new-row)))

(define (safe? positions)
  (and (not (last-item-duplicate positions))
       (not (last-item-cross positions))))

(define (last-item-duplicate positions)
  (if (= (length positions) 1)
      #f
      (if (= (length (item-in-sequence (last-element positions)
                                       positions))
             1)
          #f
          #t)))

(define (item-in-sequence item seq)
  (filter (lambda (x) (= x item))
          seq))

(define (last-element raw-list)
  (list-ref raw-list
            (- (length raw-list) 1)))

(define (last-item-cross positions)
  (or (last-item-cross-down positions) (last-item-cross-up positions)))

(define (last-item-cross-down positions)
  (contain-same-item (remove-last-item positions)
                     (possible-cross-down (- (length positions) 1)
                                          (last-element positions))))

(define (last-item-cross-up positions)
  (contain-same-item (remove-last-item positions)
                     (possible-cross-up (- (length positions) 1)
                                        (last-element positions))))

(define (contain-same-item seq-0 seq-1)
  (if (and (null? seq-0) (null? seq-1))
      #f
      (if (= (car seq-0) (car seq-1))
          #t
          (contain-same-item (cdr seq-0) (cdr seq-1)))))

(define (possible-cross-down size item)
  (if (= size 0)
      nil
      (append (list (- item size))
              (possible-cross-down (- size 1) item))))

(define (possible-cross-up size item)
  (if (= size 0)
      nil
      (append (list (+ item size))
              (possible-cross-up (- size 1) item))))

(define (remove-last-item seq)
  (define (remove-last-item-iter seq-0 len)
    (if (= len 1)
        nil
        (append (list (car seq-0))
                (remove-last-item-iter (cdr seq-0) (- len 1)))))
  (remove-last-item-iter seq (length seq)))


(queens 8)
;(length (queens 8))