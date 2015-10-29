#lang racket

(provide (all-defined-out))

(define nil '())

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


(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value)
    (newline))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- PROBE" request))
          ))
  (connect connector me)
  me)

(define (make-connector)
  (let ((value false)
        (informant false)
        (constraints nil))
    (define (set-my-value new-val setter)
      (cond ((not (has-value? me))
             (set! value new-val)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value new-val))
             (error "Contradiction" (list value new-val)))
            (else
             'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (when (not (memq new-constraint constraints))
        (set! constraints
              (cons new-constraint constraints)))
      (when (has-value? me)
        (inform-about-value new-constraint)))
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value)
             value)
            ((eq? request 'set-value!)
             set-my-value)
            ((eq? request 'forget)
             forget-my-value)
            ((eq? request 'connect)
             connect)
            (else
             (error "Unknown operation -- CONNECTOR" request))
            ))
    me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items)
           (newline))
          ((eq? (car items)
                exception)
           (loop (cdr items)))
          (else
           (procedure (car items))
           (loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))

(define (get-value connector)
  (connector 'value))

(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

(define (forget-value! connector retractor)
  ((connector 'forget) retractor))

(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))