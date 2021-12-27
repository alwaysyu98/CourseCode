#lang planet neil/sicp

(define (cont-frac n d k)
  (define (iter n d k acc)
    (if (= k 0) acc
        (iter n d (- k 1) (/ (n k) (+ (d k) acc)))))
  (iter n d k 0))

(define (approx-e)
  (cont-frac (lambda (i) 1.0)
             (lambda (i)
               (cond ((= (remainder (+ i 1) 3) 0) (* 2 (/ (+ i 1) 3)))
                     (else 1)))
             100))
(approx-e)