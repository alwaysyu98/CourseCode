#lang planet neil/sicp

(define (gcd a b)
  (if (= b 0) a
      (gcd b (remainder a b))))

; 1) normal-order evaluation

; (gcd 206 40) -> if
; (gcd 40 (remainder 206 40))
; -> (if (= (remainder 206 40) 0))
; (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
; -> (if (= (remainder 40 (remainder 206 40)) 0))
; (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
; -> (if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0))
; ...

; left right   l r
; 206  40   -> 0 0 remainder in the expression
; 40   6    -> 0 1
; 6    4    -> 1 1+0+1=2
; 4    2    -> 2 1+1+2=4
; 2    0    -> 4 1+2+4=7

; ***** so total times of remainder are performed is (1+2+4+7 for "if") + (4 times for "a") = 18,  *****


; 2) applicative-order
; (gcd 206 40) -> if
; (gcd 40 (remainder 206 40))
; (gcd 40 6)
; ...

; 206 40 -> 0 remainder
; 40 6   -> 1 remainder
; 6 4    -> 1 remainder
; 4 2    -> 1 remainder
; 2 0    -> 1 remainder

; ***** so total times of remainder are performed is 4 *****
