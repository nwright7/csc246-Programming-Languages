#lang racket
;Question1
(define char_to_number_dict (list (cons 'a 1)
                                  (cons 'b 2)
                                  (cons 'c 3)
                                  (cons 'd 4)
                                  (cons 'e 5)
                                  (cons 'f 6)))
char_to_number_dict

(set! char_to_number_dict (append char_to_number_dict (cons (cons 'g 7) null)))

char_to_number_dict

(set! char_to_number_dict (cons (cons 'z 26) char_to_number_dict))

char_to_number_dict

(dict-set char_to_number_dict 'g 99)  ; changed not saved, no ! after dict-set

char_to_number_dict

(set! char_to_number_dict (dict-remove char_to_number_dict 'g))

char_to_number_dict

(cdr (assoc 'c char_to_number_dict))

(printf "Is there an entry with key ~a? ~a\n" 'k (if (assoc 'k char_to_number_dict) "Yes" "No"))

;Question2
(define (score_to_letter score)
  (cond [(>= score 90) 'A]
        [(>= score 80) 'B]
        [(>= score 70) 'C]
        [(>= score 60) 'D]
        [else          'F]))

(define scores '(89 76 23 87 89 90 100 78 87 60 70 50 80 0 67 45))

(define (frequency lst)
  (if (null? lst)
      '()
      (letrec ([f (lambda (acc lst)
                    (if (null? lst)
                        acc
                        (let ([letter (score_to_letter (car lst))])
                          (if (dict-has-key? acc letter)
                              (f (dict-set acc letter (add1 (dict-ref acc letter))) (cdr lst))
                              (f (dict-set acc letter 1) (cdr lst))))
                        ))])
        (f '() lst))))
(frequency scores)

;Question3
(define (first_n_factorials n)
  (letrec ([f (lambda (acc i n)
                (if (= i n)
                    acc
                    (f (dict-set acc i (* i (dict-ref acc (sub1 i)))) (add1 i) n)))])
    (f '((0 . 1) (1 . 1)) 2 n))
  )

(first_n_factorials 20)

;Question4
(define (factorial_memo n)
  (letrec ([memo null]  ;a memo (dictionary-based cache) used to hold previous results
           [factorial (lambda (n)
                        (let ([ans (assoc n memo)]) ;check if already calculated
                          (if ans
                              (cdr ans)             ;if yes, return the found result directly
                              (let ([new-ans (if (or (zero? n) (= n 1));otherwise, calculate it
                                                 1
                                                 (* n (factorial (- n 1))))])
                                (begin
                                  ;add the new result to the cache before returning the answer
                                  (set! memo (append memo (cons (cons n new-ans) null))) 
                                  new-ans
                                  )))))])
    (begin
      (factorial n)
      memo)))

(factorial_memo 10) ;fast even for large number