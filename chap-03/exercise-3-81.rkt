#lang racket

(require (only-in "../ToolBox/AbstractionOfData/stream.rkt"
                  cons-stream
                  stream-map
                  stream-ref
                  display-stream-until))

(define random-init 79)

(define (rand-update x)
  (remainder (* x 973)
             37))

(define random-num
  (cons-stream random-init
               (stream-map rand-update random-num)))

(define rand
  (let ((x random-init))
    (define (dispatch command)
      (cond ((eq? command 'generate)
             (cons-stream x
                          (stream-map rand-update
                                      (rand 'generate))))
            ((eq? command 'reset)
             (lambda (new-value)
               (cons-stream new-value
                            ((rand 'reset) (rand-update new-value)))))))
    dispatch))

(display-stream-until (rand 'generate)
                      5)
(newline)

(display-stream-until ((rand 'reset) (stream-ref (rand 'generate)
                                                 2))
                      4)