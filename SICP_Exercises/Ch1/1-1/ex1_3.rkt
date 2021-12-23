#lang planet neil/sicp
(define (ex1-3 a b c)
  (define (square a) (* a a))
  (cond ((and (<= a b) (<= a c)) (+ (square b) (square c)))
        ((and (<= b a) (<= b c)) (+ (square a) (square c)))
        (else (+ (square a) (square b)))))