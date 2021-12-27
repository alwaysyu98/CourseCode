#lang planet neil/sicp

; iterative
(define (filtered-accumulate-recu filter combiner null-value term a next b) 
  (cond ((> a b) null-value)
        ((filter a) (filtered-accumulate-recu filter combiner null-value  term (next a) next b))
        (else (combiner (term a) (filtered-accumulate-recu filter combiner null-value  term (next a) next b)))))

; recursive
(define (filtered-accumulate-iter filter combiner null-value term a next b acc)
  (cond ((> a b) (combiner null-value acc))
        ((filter a) (filtered-accumulate-iter filter combiner null-value  term (next a) next b acc))
        (else (filtered-accumulate-iter filter combiner null-value term (next a) next b (combiner acc (term a))))))

; define sum
(define (sum-iter a b)
  (filtered-accumulate-iter (lambda (x) false) + 0 (lambda (x) x) a (lambda (x) (+ x 1)) b 0))

(define (sum-recu a b)
  (filtered-accumulate-recu (lambda (x) false) + 0 (lambda (x) x) a (lambda (x) (+ x 1)) b))

; test cases
(sum-iter 1 5)
(sum-recu 1 5)

(sum-iter 3 7)
(sum-recu 3 7)

