#lang racket

(require "pmatch.rkt")

; my understanding: CPS is a way to make state transformation representation more explicitly

(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only
	    (error 'empty-k "You can only invoke the empty continuation once")
	    (begin (set! once-only #t) v))))))

; 1
(define binary-to-decimal-cps
  (lambda (n k)
    (cond
      [(null? n) (k 0)]
      [else (binary-to-decimal-cps (cdr n) (lambda (x) (k (+ (car n) (* 2 x)))))])))

(binary-to-decimal-cps '() (empty-k))
(binary-to-decimal-cps '(1) (empty-k))
(binary-to-decimal-cps '(0 1) (empty-k))
(binary-to-decimal-cps '(1 1 0 1) (empty-k))

; 2
(define times-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) (k 0)]
      [else (times-cps (cdr ls) (lambda (x) (k (* (car ls) x))))])))
(times-cps '(1 2 3 4 5) (empty-k))
(times-cps '(1 2 3 0 3) (empty-k))

; 3
(define times-cps-shortcut
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) 0]
      [else (times-cps-shortcut (cdr ls) (lambda (x) (k (* (car ls) x))))])))
(times-cps-shortcut '(1 2 3 4 5) (empty-k))
(times-cps-shortcut '(1 2 3 0 3) (empty-k))

; 4
(define plus-cps
  (lambda (m k1)
    (lambda (n k2)
            (k1 (k2 (+ m n))))))

((plus-cps 2 (empty-k)) 3 (empty-k))
((plus-cps ((plus-cps 2 (empty-k)) 3 (empty-k)) (empty-k)) 5 (empty-k))

; 5
(define remv-first-9*-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k '())]
      [(pair? (car ls))
       (remv-first-9*-cps (car ls)
                          (lambda (x)
                             (cond
                               [(equal? (car ls) x)
                                (remv-first-9*-cps (cdr ls) (lambda (x*) (k (cons (car ls) x*))))]
                               [else (remv-first-9*-cps (car ls) (lambda (x*) (k (cons x* (cdr ls)))))])))]
      [(eqv? (car ls) '9) (k (cdr ls))]
      [else (remv-first-9*-cps (cdr ls) (lambda (x) (k (cons (car ls) x))))])))

(remv-first-9*-cps '((1 2 (3) 9)) (empty-k))
(remv-first-9*-cps '(9 (9 (9 (9)))) (empty-k))
(remv-first-9*-cps '(((((9) 9) 9) 9) 9) (empty-k))

; 6
(define cons-cell-count
  (lambda (ls)
    (cond
      [(pair? ls)
       (add1 (+ (cons-cell-count (car ls)) (cons-cell-count (cdr ls))))]
      [else 0])))

(define cons-cell-count-cps
  (lambda (ls k)
    (cond
      [(pair? ls)
       (cons-cell-count-cps (car ls)
                            (lambda (x)
                               (cons-cell-count-cps (cdr ls)
                                                    (lambda (y)
                                                      (k (add1 (+ x y)))))))]
      [else (k 0)])))
(cons-cell-count '(1 2 3 4 5))
(cons-cell-count '(1 2 3))
(cons-cell-count '())
(cons-cell-count-cps '(1 2 3 4 5) (empty-k))
(cons-cell-count-cps '(1 2 3) (empty-k))
(cons-cell-count-cps '() (empty-k))

; 7
(define find-cps
  (lambda (u s k)
    (let ((pr (assv u s)))
      (if pr (find-cps (cdr pr) s k) (k u)))))

(find-cps 5 '((5 . a) (6 . b) (7 . c)) (empty-k))
(find-cps 7 '((5 . a) (6 . 5) (7 . 6)) (empty-k))
(find-cps 5 '((5 . 6) (9 . 6) (2 . 9)) (empty-k))

; 8
(define ack-cps
  (lambda (m n k)
    (cond
      [(zero? m) (k (add1 n))]
      [(zero? n) (ack-cps (sub1 m) 1 k)]
      [else (ack-cps m (sub1 n)
                     (lambda (x)
                       (ack-cps (sub1 m) x k)))])))

(ack-cps 3 2 (empty-k))

; 9
(define fib-cps
  (lambda (n k1)
     ((lambda (fib k2)
        (fib fib n k2))
      (lambda (fib n k3)
        (cond
          [(zero? n) (k3 0)]
          [(= 1 n) (k3 1)]
          [else
           (fib fib (sub1 n) (lambda (x)
                               (fib fib (sub1 (sub1 n)) (lambda (y)
                                                          (k3 (+ x y))))))])) k1)))
(fib-cps 5 (empty-k))
(fib-cps 6 (empty-k))
(fib-cps 7 (empty-k))

; 10
; (case1) if it is an element (function, number, symbol etc.), than apply continuation
; (case2) otherwise, consider complete it with continuation.
; use the words in notes,
; (case1) rule1 -> process the body
; (case2) rule2 -> pass it to k
(define unfold-cps
  (lambda (p f g seed k1)
    ((lambda (h k2)
       (h h (lambda (x)
              (x seed '() k2))))

     (lambda (h k3)
       (k3 (lambda (seed ans k4)
             (p seed (lambda (condition)
                       (if condition
                           (k4 ans)
                           (h h (lambda (x)
                                  (g seed (lambda (y)
                                            (f seed (lambda (z)
                                                      (x y (cons z ans) k4)))))))))))))
     k1)))

(define null?-cps
    (lambda (ls k)
      (k (null? ls))))
(define car-cps
    (lambda (pr k)
      (k (car pr))))
(define cdr-cps
    (lambda (pr k)
      (k (cdr pr))))
(unfold-cps null?-cps car-cps cdr-cps '(a b c d e) (empty-k))

; 11
(define empty-s
  (lambda ()
    '()))
 
(define unify-cps
  (lambda (u v s k)
    (cond
      ((eqv? u v) (k s))
      ((number? u) (k (cons (cons u v) s)))
      ((number? v) (unify-cps v u s k))
      ((pair? u)
       (if (pair? v)
           (find-cps (car u) s (lambda (x)
                                 (find-cps (car v) s (lambda (y)
                                                       (unify-cps x y s (lambda (z)
                                                                          (if z
                                                                              (find-cps (cdr u) z (lambda (x^)
                                                                                                    (find-cps (cdr v) z (lambda (y^)
                                                                                                                          (unify-cps x^ y^ z k)))))
                                                                              (k #f))))))))
                                                                                                                      
           (k #f)))
      (else (k #f)))))

(unify-cps 'x 5 (empty-s) (empty-k))
(unify-cps 'x 5 (unify-cps 'y 6 (empty-s) (empty-k)) (empty-k))
(unify-cps '(x y) '(5 6) (empty-s) (empty-k))
(unify-cps 'x 5 (unify-cps 'x 6 (empty-s) (empty-k)) (empty-k))
(unify-cps '(x x) '(5 6) (empty-s) (empty-k))
(unify-cps '(1 2 3) '(x 1 2) (empty-s) (empty-k))
(unify-cps 'x 'y (empty-s) (empty-k))

; 12
(define M-cps
  (lambda (f k1)
    (k1 (lambda (ls k2)
          (cond
            ((null? ls) (k2 '()))
            (else
             (f (car ls) (lambda (x)
                           (M-cps f (lambda (y)
                                      (y (cdr ls) (lambda (z)
                                                    (k2 (cons x z))))))))))))))

((M-cps car-cps (empty-k)) '((1 2) (3 4) (5 6)) (empty-k))

; 13
(define use-of-M-cps
  ((M-cps car-cps (empty-k)) '((1 2) (3 4) (5 6)) (empty-k)))
