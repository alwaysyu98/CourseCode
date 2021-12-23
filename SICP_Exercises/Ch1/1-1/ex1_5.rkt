#lang planet neil/sicp
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

; for applicative-order evaluation, the interpreter evaluates when values are needed.
; (test 0 (p)) -> (test 0 (p)) -> ... 

; for normal-order evaluation, the interpreter will "fully expand and then reduce".
; (test 0 (p)) -> (if (= 0 0) 0 (p)) -> (if #t 0 (p)) -> 0
