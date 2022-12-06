#lang planet neil/sicp

; iterative
(define (accumulate-recu combiner null-value term a next b) 
  (if (> a b) null-value
      (combiner (term a) (accumulate-recu combiner null-value  term (next a) next b)))) 

; recursive
(define (accumulate-iter combiner null-value term a next b acc)
  (if (> a b) (combiner null-value acc)
      (accumulate-iter combiner null-value term (next a) next b (combiner acc (term a)))))

; define sum
(define (sum-iter a b)
  (accumulate-iter + 0 (lambda (x) x) a (lambda (x) (+ x 1)) b 0))

(define (sum-recu a b)
  (accumulate-recu + 0 (lambda (x) x) a (lambda (x) (+ x 1)) b))

; test cases
(sum-iter 1 5)
(sum-recu 1 5)

(sum-iter 3 7)
(sum-recu 3 7)

; define product
(define (product-iter a b)
  (accumulate-iter * 1 (lambda (x) x) a (lambda (x) (+ x 1)) b 1))

(define (product-recu a b)
  (accumulate-recu * 1 (lambda (x) x) a (lambda (x) (+ x 1)) b))

; test cases
(product-iter 1 5)
(product-recu 1 5)

(product-iter 3 7)
(product-recu 3 7)
