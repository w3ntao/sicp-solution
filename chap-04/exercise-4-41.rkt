#lang racket

(require (only-in "../toolBox/AbstractionOfData/list.rkt"
                  nil
                  flatmap
                  list-ref))

(provide (combine-out apply-constrains))

(define (all-combination raw-list)
  (if (null? raw-list)
      (list nil)
      (flatmap (lambda (x)
                 (insert-item-at-every-position x
                                                (car raw-list)))
               (all-combination (cdr raw-list)))))

(define (insert-item-at-every-position raw-list item)
  (define (insert-item n)
    (append (list (insert-item-at raw-list
                                  n))
            (if (= n 0)
                nil
                (insert-item (- n 1)))))
  (define (insert-item-at temp-list n)
    (if (= n 0)
        (cons item
              temp-list)
        (cons (car temp-list)
              (insert-item-at (cdr temp-list)
                              (- n 1)))))
  (insert-item (length raw-list)))

(define (apply-constrains combination constrains)
  (if (null? constrains)
      combination
      (apply-constrains (filter (car constrains)
                                combination)
                        (cdr constrains))))

(define (multiple-dewelling)
  (let ((Baker    0)
        (Cooper   1)
        (Fletcher 2)
        (Miller   3)
        (Smith    4))
    (define (floor-index floor-list Name)
      (list-ref floor-list Name))
    (define constrain-list
      (list (lambda (floor-list)
              (not (= (floor-index floor-list Baker)
                      5)))
            (lambda (floor-list)
              (not (= (floor-index floor-list Cooper)
                      1)))
            (lambda (floor-list)
              (not (or (= (floor-index floor-list Fletcher)
                          1)
                       (= (floor-index floor-list Fletcher)
                          5))))
            (lambda (floor-list)
              (> (floor-index floor-list Miller)
                 (floor-index floor-list Cooper)))
            (lambda (floor-list)
              (not (= (abs (- (floor-index floor-list Smith)
                              (floor-index floor-list Fletcher)))
                      1)))
            (lambda (floor-list)
              (not (= (abs (- (floor-index floor-list Fletcher)
                              (floor-index floor-list Cooper)))
                      1)))))

    (define one-to-five (list 1 2 3 4 5))

    (map (lambda (x)
           (display "Baker:    ")
           (display (floor-index x Baker))
           (newline)
           (display "Cooper:   ")
           (display (floor-index x Cooper))
           (newline)
           (display "Fletcher: ")
           (display (floor-index x Fletcher))
           (newline)
           (display "Miller:   ")
           (display (floor-index x Miller))
           (newline)
           (display "Smith:    ")
           (display (floor-index x Smith))
           (newline)
           (newline))
         (apply-constrains (all-combination one-to-five)
                      constrain-list))))

(multiple-dewelling)