#lang racket
;Question1

(define (letter_grade score)
  (cond [(>= score 90) 'A]
        [(>= score 80) 'B]
        [(>= score 70) 'C]
        [(>= score 60) 'D]
        [#t 'F]))

(letter_grade 77)

;Question2
(let* ([x 5]
       [y (* x 10)])
  (+ x y))

;Question3
(letrec ([sum (lambda (m n)
                (if (= m n)
                    m
                    (+ m (sum (add1 m) n))))])
  (sum 2 10))

;Question4
(display "Please type in integer within [0-100]:\n")
(define score (read))
(letter_grade score)
(display "Please type another integer within [0-100]:\n")
(set! score (read))
(letter_grade score)
