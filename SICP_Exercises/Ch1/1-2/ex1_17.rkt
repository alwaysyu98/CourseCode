#lang planet neil/sicp


; custom "*" operation
(define (* a b)
  ; just to simulate operations
  (define (double x)
    (+ x x))
  (define (halve x)
    (/ x 2))
  (cond ((= b 0) 0)
        ((= (remainder b 2) 0) (* (double a) (halve b)))
        (else (+ a (* a (- b 1))))))

; test case
(* 1 4)
(* 3 5)
(* 5 4)
(* 2 4)
