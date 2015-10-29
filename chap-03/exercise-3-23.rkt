#lang racket

(require (only-in (combine-in rnrs/base-6
                              rnrs/mutable-pairs-6)
                  cons
                  car
                  cdr
                  set-car!
                  set-cdr!))

(define nil '())

(define (make-deque)
  (cons nil nil))

(define (front-ptr deque)
  (car deque))

(define (rear-ptr deque)
  (cdr deque))

(define (empty-deque? deque)
  (and (null? (front-ptr deque))
       (null? (rear-ptr deque))))

(define (set-front-ptr! deque item)
  (set-car! deque item)
  deque)

(define (set-rear-ptr! deque item)
  (set-cdr! deque item))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "empty deque" deque)
      (car (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "empty deque" deque)
      (car (rear-ptr deque))))


(define (front-insert-deque! deque item)
  (let ((new-pair (cons item nil)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
    (else
     (set-cdr! new-pair (front-ptr deque))
     (set-front-ptr! deque new-pair)
     deque))))

(define (rear-insert-deque! deque item)
  (let ((new-pair (cons item nil)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
    (else
     (set-cdr! (rear-ptr deque) new-pair)
     (set-rear-ptr! deque new-pair)
     deque))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "empty deque" deque))
        (else
         (set-front-ptr! deque (cdr (front-ptr deque)))
         deque)))

(define (print-queue queue)
  (display (front-ptr queue)))

(define test-0 (make-deque))

(front-insert-deque! test-0 'a)
(front-insert-deque! test-0 'b)
(front-insert-deque! test-0 'c)
(rear-insert-deque! test-0 0)

(newline)
(print-queue test-0)
(newline)

(print-queue (front-delete-deque! test-0))