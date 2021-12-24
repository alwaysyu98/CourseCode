#lang planet neil/sicp

; Pascle's triangle

; row m, column n
; 1-index
(define (pascal-triangle m n)
  (define (pascal-triangle-recursive m n)
    (cond ((or (= m 1) (= 1 n) (= m n)) 1)
           ((or (> n m) (< n 1)) 0)
           (else (+ (pascal-triangle-recursive (- m 1) n)
              (pascal-triangle-recursive (- m 1) (- n 1))))))
  (if (or (> n m) (< n 1))
      (display "sorry the input parameters is wrong")
      (pascal-triangle-recursive m n)))

; test case
;    1
;   1 1
;  1 2 1
; 1 3 3 1
;1 4 6 4 1

; 1
(pascal-triangle 1 1)
; 1
(pascal-triangle 2 2)
; 2
(pascal-triangle 3 2)
; 3
(pascal-triangle 4 2)
; 6
(pascal-triangle 5 3)
          
