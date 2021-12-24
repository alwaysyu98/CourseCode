#lang planet neil/sicp

; A(x, y) = 0 , y == 0
; A(x, y) = 2y, x == 0
; A(x, y) = 2 , y == 1
; A(x, y) = A(0, A(x, y-1)) = 2*A(x, y-1) = 2^y, (x == 1)

; A(x-1, 2) = A(x-2, A(x-1, 1)) = A(x-2, 2) = ... = 4
; A(x-1, 3) = A(x-2, A(x-1, 2)) = A(x-2, 4) = ... = 4
; A(x-1, 4) = A(x-2, A(x-1, 3)) = A(x-2, 2) = ... = 4

; A(x, y) = A(x-1, A(x, y-1)) , x >= 1
;   = A(x-1, A(x-1, A(x-1, y-2))) = ... = A(x-1, A(x-1, ..., A(x-1, y-n)..))
;   = A(x-1, A(x-1, ..., (2)..)) = A(x-1, A(x-1, ..., A(x-1 ,2)..)) = A(x-1, A(x-1, ..., A(x-1 ,2)..))
;   = 2^(y)


(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)

(A 2 4)

(A 3 3)


; (f n) = (* 2 n) = 2*n
(define (f n) (A 0 n))

; (g n) = (* 2 (g n-1)) = 2^n
(define (g n) (A 1 n))

; (g n) = (pow 2 (g n-1)) = 2^(2^...2)..) [n 2]
(define (h n) (A 2 n))

(define (k n) (* 5 n n))