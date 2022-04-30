#lang racket
(define numbers (mcons 4 (mcons 5 (mcons 6 (mcons 8 null)))))

(define (frequency lst)
  (if (null? lst)
      '()
      (letrec ([tr_freq (lambda (acc lst)
                          (if (null? lst)
                              acc
                              (if (dict-has-key? acc (car lst))
                                  (tr_freq (dict-set acc (car lst) (add1 (dict-ref acc (car lst)))) (cdr lst))
                                  (tr_freq (dict-set acc (car lst) 1) (cdr lst)))
                                  )
                          )])
        ;call the local tail recursive procedure
        (tr_freq '() lst))))

(define chars (list 'a 'b 'a 'c 'd 'a))

(define charFreqDict (frequency chars))

charFreqDict