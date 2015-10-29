#lang racket

(require (combine-in (only-in "../ToolBox/AbstractionOfData/queue.rkt"
                              nil
                              make-queue
                              empty-queue?
                              insert-queue!
                              delete-queue!
                              print-queue)))

(define q1 (make-queue))

(insert-queue! q1 'a)

(insert-queue! q1 'b)

(insert-queue! q1 'c)

(print-queue (delete-queue! q1))