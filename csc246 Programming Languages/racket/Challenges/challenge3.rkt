#lang racket
;Question1
;You can assume the user won't type strange inputs.
(display "Please specify an operation to perform (+, -, *, /): \n")
(define operation (read))
(display "Please specify the first operand:\n")
(define num1 (read))
(display "Please specify the second operand:\n")
(define num2 (read))
(printf "The result is ~a.\n" (cond [(eq? operation '+) (+ num1 num2)]
                                    [(eq? operation '-) (- num1 num2)]
                                    [(eq? operation '*) (* num1 num2)]
                                    [(eq? operation '/) (/ num1 num2)]))
;Question2
(letrec ([average (lambda (acc count n)
                    (if (= count n)
                        (/ acc n)
                        (average (+ acc (read)) (add1 count) n)))])
  (printf "The average is ~a.\n" (average 0 0 5)))

;Question3
(define (alternateOddEven lst) 
  (letrec ([expect_odd (lambda (lst)
                         (if (null? lst)
                             #t
                             (if (odd? (car lst))
                                 (expect_even (cdr lst))
                                 #f)))]
           [expect_even (lambda (lst)
                          (if (null? lst)
                              #t
                              (if (even? (car lst))
                                  (expect_odd (cdr lst))
                                  #f)))])
    (or (expect_odd lst) (expect_even lst)))
  )
(alternateOddEven '(1 2 3 5 5))

;Question4
(letrec ([readData (lambda (acc count)
                    (if (zero? count)
                        acc
                        (readData (mcons (read) acc) (sub1 count))))]
         [sum (lambda (mlst)
                (if (null? mlst)
                    0
                    (+ (mcar mlst) (sum (mcdr mlst)))))])
  (printf "The sum is ~a.\n" (sum (readData '() 5))))