#lang planet neil/sicp

; f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n>=3

(define (f-recursive n)
  (if (< n 3) n
      (+ (f-recursive (- n 1))
          (* 2 (f-recursive (- n 2)))
          (* 3 (f-recursive (- n 3))))))

(define (f-iterative n)
  (define (f n fn-1 fn-2 fn-3)
    (let ((tmp (+ fn-1 (* 2 fn-2) (* 3 fn-3))))
    (if (= n 0)
        tmp
        (f (- n 1) tmp fn-1 fn-2))))
  (if (< n 3) n
      (f (- n 3) 2 1 0)))

; test 
(f-recursive 5)
(f-iterative 5)

(f-recursive 10)
(f-iterative 10)