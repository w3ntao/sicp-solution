#lang racket

(provide (all-defined-out))

(require (only-in "connector.rkt"
                  make-connector
                  set-value!
                  has-value?
                  get-value
                  forget-value!
                  connect
                  constant
                  probe))

(define (adder a-0 a-1 sum)
  (define (process-new-value)
    (cond ((and (has-value? a-0)
                (has-value? a-1))
           (set-value! sum
                       (+ (get-value a-0)
                          (get-value a-1))
                       me))
          ((and (has-value? a-0)
                (has-value? sum))
           (set-value! a-1
                       (- (get-value sum)
                          (get-value a-0))
                       me))
          ((and (has-value? a-1)
                (has-value? sum))
           (set-value! a-0
                       (- (get-value sum)
                          (get-value a-1))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a-0 me)
    (forget-value! a-1 me)
    (process-new-value))

  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- ADDER" request))))

  (connect a-0 me)
  (connect a-1 me)
  (connect sum me)
  me)

(define (multiplier m-0 m-1 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m-0)
                    (= (get-value m-0)
                       0))
               (and (has-value? m-1)
                    (= (get-value m-1)
                       0)))
           (set-value! product 0 me))
          ((and (has-value? m-0)
                (has-value? m-1))
           (set-value! product
                       (* (get-value m-0)
                          (get-value m-1))
                       me))
          ((and (has-value? product)
                (has-value? m-0))
           (set-value! m-1
                       (/ (get-value product)
                          (get-value m-0))
                       me))

          ((and (has-value? product)
                (has-value? m-1))
           (set-value! m-0
                       (/ (get-value product)
                          (get-value m-1))
                       me))))
  (define (process-forget-value)
    (forget-value! m-0 me)
    (forget-value! m-1 me)
    (forget-value! product me))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- MULTIPLIER" request))))
  (connect m-0 me)
  (connect m-1 me)
  (connect product me)
  me)