#lang planet neil/sicp

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

; test cases
(define (cube x)
  (* x x x))

(sum cube 1 (lambda (x) (+ x 1)) 4)

    
