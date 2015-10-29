#lang racket

(provide (all-defined-out))

(require (only-in (combine-in rnrs/base-6
                              rnrs/mutable-pairs-6)
                  cons
                  car
                  cdr
                  set-car!
                  set-cdr!))

(define nil '())

(define (make-queue)
  (cons nil nil))

(define (front-ptr queue)
  (car queue))

(define (rear-ptr queue)
  (cdr queue))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (set-front-ptr! queue item)
  (set-car! queue item)
  queue)

(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item nil)))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

(define (print-queue queue)
  (display (front-ptr queue)))