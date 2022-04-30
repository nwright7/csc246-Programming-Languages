#lang racket

;Normal recursive implementation - slow for big n's
;(define (fibanocci n)
;  (if (or (zero? n) (= n 1))
;      n
;      (+ (fibanocci (- n 1)) (fibanocci (- n 2))))
;  )
;(fibanocci 40) ;slow

(define (fibanocci_memo n)
  (letrec ([memo null]  ;a memo (dictionary-based cache) used to hold previous results
           [fibanocci (lambda (n)
                        (let ([ans (assoc n memo)]) ;check if already calculated
                          (if ans
                              (cdr ans)             ;if yes, return the found result directly
                              (let ([new-ans (if (or (zero? n) (= n 1));otherwise, calculate it
                                                 n
                                                 (+ (fibanocci (- n 1)) (fibanocci (- n 2))))])
                                (begin
                                  ;add the new result to the cache before returning the answer
                                  (set! memo (cons (cons n new-ans) memo)) 
                                  new-ans
                                  )))))])
    (fibanocci n)))

(fibanocci_memo 1000) ;fast even for large number