#lang planet neil/sicp

; custom "*" operation
; minus equals add a minus number so do not specify it extra
(define (* a b)
  (define (double x)
    (+ x x))
  (define (halve x)
    (/ x 2))
  (define (*-iter a b ans)
    (cond ((= b 0) ans)
          ((= (remainder b 2) 1) (*-iter (double a) (halve (- b 1)) (+ ans a)))
          (else (*-iter (double a) (halve b) ans))))
  (*-iter a b 0))

; test case
(* 1 4)
(* 3 5)
(* 5 4)
(* 2 4)
