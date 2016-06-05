#lang racket

(define nil '())

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (dot-product v w);both v and w are vectors
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x)
         (dot-product x v))
       m))


(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op
                        init
                        (get-first-row seqs))
            (accumulate-n op init (get-remain-row seqs)))))

(define (get-first-row seq)
  (if (null? seq)
      nil
      (cons (car (car seq))
            (get-first-row (cdr seq)))))

(define (get-remain-row seq)
  (if (null? seq)
      nil
      (cons (cdr (car seq))
            (get-remain-row (cdr seq)))))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x)
           (matrix-*-vector cols x))
         m)))


(define test-matrix (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))

(define matrix-2 (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

(define test-vec (list 1 2 3 4))

;(dot-product test-vec test-vec)

;(matrix-*-vector test-matrix test-vec)

;(transpose matrix-2)

(matrix-*-matrix matrix-2 matrix-2)


;(map * (list 1 2 3) (list 1 2 3))