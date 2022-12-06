#lang planet neil/sicp

; iterative
(define (product-recu term a next b) 
  (if (> a b) 1
      (* (term a) (product-recu term (next a) next b)))) 

; recursive
(define (product-iter term a next b product) 
  (if (> a b) product
      (product-iter term (next a) next b (* product (term a))))) 
    
; factorial
(define (factorial-recu n)
  (product-recu (lambda (x) x) 1 (lambda (x) (+ x 1)) n))

(define (factorial-iter n)
  (product-iter (lambda (x) x) 1 (lambda (x) (+ x 1)) n 1))

; cal pi
(define (cal-pi)
  (/ 2
     (/ (product-iter (lambda (x) x) 4 (lambda (x) (+ x 2)) 1000 1)
     (product-iter (lambda (x) x) 3 (lambda (x) (+ x 2)) 1000 1))))

; test cases
(factorial-recu 6)
(factorial-recu 5)
(factorial-recu 4)
(factorial-recu 3)
(factorial-recu 2)
(factorial-recu 1)

(factorial-iter 6)
(factorial-iter 5)
(factorial-iter 4)
(factorial-iter 3)
(factorial-iter 2)
(factorial-iter 1)