#lang planet neil/sicp

; b^n = (b^(n/2))^2, n is even
; b^n = b*b^(n-1), n is odd

; Quoted from the book:
; In general, the technique of defining an invariant quantity
;that remains unchanged from state to state is a powerful way
; to think about the design of iterative algorithms.

; It is same with the book idea -- to keep the invariant quantity
(define (fast-exp b n)
  (define (odd? n)
    (= (remainder n 2) 1))
  (define (square x) (* x x))
  (define (fast-exp-iter n p ans)
          ((odd? n) (fast-exp-iter (/ (- n 1) 2) (* p p) (* ans p)))
          (else (fast-exp-iter (/ n 2) (* p p) ans))))
  (fast-exp-iter n b 1))

; test case
(fast-exp 2 3)
(fast-exp 2 4)
(fast-exp 4 0)
(fast-exp 4 1)
(fast-exp 4 2)
(fast-exp 4 3)
(fast-exp 4 4)
(fast-exp 4 5)