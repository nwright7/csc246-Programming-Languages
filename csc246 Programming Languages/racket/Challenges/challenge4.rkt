#lang racket
;Question1
(define n (read))
(cond [(even? n) (printf "~a is even.\n" n)]
      [else      (printf "~a is odd.\n" n)])

(display "UnThunked version:\n")
(define (func_with_cond condition
                        true_case_expression
                        false_case_expression)
  (if condition true_case_expression false_case_expression))
;It runs but gives you confusing result since both expressions are evaluated.
(func_with_cond (even? n) (printf "~a is even.\n" n) (printf "~a is odd.\n" n))

;Question2
(display "Thunked version:\n")
(define (func_with_cond_thunk  condition
                               true_case_thunk
                               false_case_thunk)
  (if condition (true_case_thunk) (false_case_thunk)))

(func_with_cond_thunk (even? n)
                      (lambda () (printf "~a is even.\n" n))
                      (lambda () (printf "~a is odd.\n" n)))

;Question3
(define multiples_of_four
  (letrec ([f (lambda (x)
                (cons x (lambda () (f (+ 4 x)))))])
    (lambda () (f 0))
    ))

;Question4
(define (first_n_from_a_stream stream n)
  (letrec ([first_n (lambda (acc stream n)
                      (if (zero? n)
                          acc
                          (first_n (append acc (cons (car (stream)) null)) (cdr (stream)) (sub1 n))))])
    (first_n '() stream n))
  )

(first_n_from_a_stream multiples_of_four 8)