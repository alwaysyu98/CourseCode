#lang planet neil/sicp

(define (cube x)
  (* x x x))

(define (simposn f a b n)
  (let ((h (/ (- b a) n)))
  (define (simposn-iter f a b n i sum)
    (let ((x (+ a  (* i h))))
      (cond ((= i 0) (simposn-iter f a b n (+ i 1) (+ sum (f x))))
            ((= i n) (+ sum (f x)))
            ((odd? i) (simposn-iter f a b n (+ i 1) (+ sum (* 4 (f x)))))
            (else (simposn-iter f a b n (+ i 1) (+ sum (* 2 (f x))))))))
  (* h (/ 1 3) (simposn-iter f a b n a 0))))

; test cases
(simposn cube 0 10 1000)
(simposn cube 0 4 10)

    
