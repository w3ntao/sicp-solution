#lang racket

(require (combine-in (only-in "../ToolBox/Math/prime.rkt"
                              square)
                     (only-in "../ToolBox/ConstraintSystem/connector.rkt"
                              make-connector
                              has-value?
                              set-value!
                              get-value
                              forget-value!
                              connect
                              probe)))

(define (squarer a b)
  (define (process-new-value)
    (cond ((has-value? b)
           (if (< (get-value b)
                  0)
               (error "square less than 0 -- SQUARER" (get-value b))
               (set-value! a
                           (sqrt (get-value b))
                           me)))
          ((has-value? a)
           (set-value! b
                       (square (get-value a))
                       me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

(define temp-a (make-connector))
(define temp-b (make-connector))

(squarer temp-a temp-b)
(probe "a" temp-a)
(probe "b" temp-b)


(set-value! temp-a 10 'usr)

(forget-value! temp-a 'usr)

(set-value! temp-b 625 'usr)