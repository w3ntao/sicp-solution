#lang racket

(require (combine-in (only-in "../ToolBox/AbstractionOfData/queue.rkt"
                              make-queue
                              empty-queue?
                              insert-queue!
                              delete-queue!
                              print-queue)))

(define test (make-queue))

(empty-queue? test)

(insert-queue! test 1)

(insert-queue! test 2)

(insert-queue! test 3)

(print-queue test)

(newline)

(delete-queue! test)

(print-queue test)