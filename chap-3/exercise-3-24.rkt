#lang racket

(require (only-in (combine-in rnrs/base-6
                              rnrs/mutable-pairs-6)
                  cons
                  car
                  caar
                  cdr
                  list
                  set-car!
                  set-cdr!))

(define nil '())

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records))
             (car records))
            (else
             (assoc key (cdr records)))))
    (define (lookup key-0 key-1)
      (let ((subtable (assoc key-0
                             (cdr local-table))))
        (if subtable
            (let ((record (assoc key-1
                                 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-0 key-1 value)
      (let ((subtable (assoc key-0 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-1 (cdr subtable))))
              (if record
                  (set-cdr! record
                            value)
                  (set-cdr! subtable
                           (cons (cons key-1
                                       value)
                                 (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-0
                                  (cons key-1
                                        value))
                            (cdr local-table)))))
        'OK)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc)
             lookup)
            ((eq? m 'insert-proc!)
             insert!)
            (else
             (error "error opr" m))))
    dispatch))

(define (approximate torelance)
  (lambda (x y)
    (< (abs (- x y))
       torelance)))

(define operation-table (make-table equal?))

(define get (operation-table 'lookup-proc))

(define put (operation-table 'insert-proc!))

(put 'math 1 '*)

(put 'math 21 '+)

(get 'math 1)

(get 'math 21)