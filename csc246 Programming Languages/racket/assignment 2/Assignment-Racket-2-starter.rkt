#lang racket

; ------------------------- Nick Wright --------------------------------

;;Warning - your program won't stop quickly since you need to improve its
;;          efficiency in Part 2. You can comment the last line out temporarily.

;;Part A - Calculate the frequencies of user input strings.

;Prompt the user to type ten strings and store them in a list
;(define userInput "") ;userInput will store the input string
(define inputStrings 
  (letrec ([strs '()]
           [readStrings (lambda (n)
                          
                          ;use (read-line (current-input-port)) to read a string
                          ;implement the logic to read n strings here
                          ;strs ;replace this with your code


                          ;check that n is not 0
                          (if (> n 0)
                              (begin
                                (printf "Please enter 10 strings to be stored in a list, remaining strings: ~a\n" n)
                                ;(set! userInput (read-line (current-input-port)))
                                ;(set! strs (append strs (list userInput)))
                                (set! strs (append strs (list (read-line (current-input-port)))))
                                (set! n (sub1 n))
                                (readStrings n)
                                )
                              ;if 10 have been typed, return the list 
                              strs)
                          
                          )])
    (readStrings 10)))



(define (lengthFreq lst)
  (letrec ([f (lambda (acc lst)
                ;implement the logic to collect the frequency of strings based upon their length
                ;you can use string-length procedure to get a string's length
                ;acc ;replace this with your code
                (if (null? lst)
                              acc
                              (if (dict-has-key? acc (string-length (car lst)))
                                  (f (dict-set acc (string-length (car lst)) (add1 (dict-ref acc (string-length (car lst))))) (cdr lst))
                                  (f (dict-set acc (string-length (car lst)) 1) (cdr lst)))
                                  )
                )])
    (f '() lst))
  )

;output interpretation:
;assume you get this output from the following procedure call
;          '((9 . 1) (3 . 5) (2 . 2) (4 . 2))
;(9 . 1) means there is one string with a length of 9
(lengthFreq inputStrings)

; --------- testing lengthFreq ----------
;(define a '("hi" "hello" "ok" "ok"))
;(lengthFreq a)


;;Part B - Use memoization to speed up the following code.
;;         Sample code demonstrated in class is given to you.

;def A(n):
;        if n < 3:
;                return 1
;        else:
;                return B(n-1) + B(n-2)

;Two dictionaries, one for A and one for B, given for you to start with.
;They have to global here so both A and B can access them
(define memoA null)
(define memoB null)

;(define (A n)
;  (if (< n 3)
;      1
;      (+ (B (sub1 n)) (B (- n 2))))
;  )
;
;(define (B n)
;  (if (< n 3)
;      2
;      (+ (A (sub1 n)) (A (- n 2))))
;  )

#||#
(define (A n)
  (letrec ([recA (lambda (n)
                  (let ([ans (assoc n memoA)]) ;check if this has already been calculated
                    (if ans
                        (cdr ans) ;if true, we return the value
                        (let ([new-ans (if (< n 3)
                                           1
                                           (+ (B (sub1 n)) (B (- n 2))))])
                          (begin
                            ;add the new result to the cache before returning the answer
                            (set! memoA (cons (cons n new-ans) memoA))
                            new-ans
                            )))))])
(recA n)))

(define (B n)
  (letrec ([recB (lambda (n)
                  (let ([ans (assoc n memoB)]) ;check if this has already been calculated
                    (if ans
                        (cdr ans) ;if true, we return the value
                        (let ([new-ans (if (< n 3)
                                           2
                                           (+ (A (sub1 n)) (A (- n 2))))])
                          (begin
                            ;add the new result to the cache before returning the answer
                            (set! memoB (cons (cons n new-ans) memoB))
                            new-ans
                            )))))])
(recB n)))

;Try calling B with 10.
(B 10)
;Then try 1000
(B 1000)