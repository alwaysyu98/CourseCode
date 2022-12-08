#lang racket

; C311 - 2016?
; Date: 7th, Dec, 2022
; Writer Jingyi Yu

; test with test tool
; (require "a1-student-tests.rkt")
; (test-file #:file-name "a1.rkt")

; 1. Define and test a procedure countdown that takes a natural number and returns a list of the natural numbers less than or equal to that number, in descending order.

(define countdown
  (lambda (x)
  (cond
    ((zero? x) '(0))
    ((cons x (countdown (- x 1)))))))
(countdown 5)

; 2. Define and test a procedure insertR that takes two symbols and a list and returns a new list with the second symbol inserted after each occurrence of the first symbol. For this and later questions, these functions need only hold over eqv?-comparable structures.

(define insertR
  (lambda (a b c)
       (cond
         ((eqv? c null) '())
         ((eqv? (car c) a) (cons a (cons b (insertR a b (cdr c)))))
         (else (cons (car c) (insertR a b (cdr c)))))))
(insertR 'x 'y '(x z z x y x))

; 3. Define and test a procedure remv-1st that takes a a symbol and a list and returns a new list with the first occurrence of the symbol removed.

(define remv-1st
  (lambda (x y)
    (cond
      ((eqv? y null) '())
      ((eqv? (car y) x) (cdr y))
      (else (cons (car y) (remv-1st x (cdr y)))))))
(remv-1st 'x '(x y z x))
(remv-1st 'y '(x y z y x))

; 4. Define and test a procedure list-index-ofv? that takes an element and a list and returns the (base 0) index of that element in the list. A list missing that element will be considered bad data.

(define list-index-ofv?
  (lambda (a b)
    (letrec
        ((helper (lambda (a b index)
                   (cond
                     ((eqv? b null) -1)
                     ((eqv? (car b) a) index)
                     (else (helper a (cdr b) (add1 index)))))))
      (helper a b 0))))
(list-index-ofv? 'x '(x y z x x))
(list-index-ofv? 'x '(y z x x))


; 5. Define and test a procedure filter that takes a predicate and a list and returns a new list containing the elements that satisfy the predicate. A predicate is a procedure that takes a single argument and returns either #t or #f. The number? predicate, for example, returns #t if its argument is a number and #f otherwise. The argument satisfies the predicate, then, if the predicate returns #t for that argument.

(define filter
  (lambda (a b)
    (cond
      ((null? b) '())
      ((a (car b)) (cons (car b) (filter a (cdr b))))
      (else (filter a (cdr b))))))
(filter even? '(1 2 3 4 5 6))

; 6. Define and test a procedure zip that takes two lists and forms a new list, each element of which is a pair formed by combining the corresponding elements of the two input lists. If the two lists are of uneven length, then drop the tail of the longer one.
(define zip
  (lambda (a b)
    (cond
      ((null? a) '())
      ((null? b) '())
      (else (cons (cons (car a) (car b)) (zip (cdr a) (cdr b)))))))
(zip '(1 2 3) '(a b c))
(zip '(1 2 3 4 5 6) '(a b c))
(zip '(1 2 3) '(a b c d e f))

; https://stackoverflow.com/questions/6006671/are-pair-and-list-different-in-scheme
; pair and list are different
; '(1 . 2) = (cons 1 2)
; '(1 2)   = (cons 1 (cons 2 nil)) 

; 7. Define and test a procedure map that takes a procedure p of one argument and a list ls and returns a new list containing the results of applying p to the elements of ls. Do not use Racket's built-in map in your definition.
(define map
  (lambda (a b)
    (cond
      ((null? b) '())
      (else (cons (a (car b)) (map a (cdr b)))))))
(map add1 '(1 2 3 4))

; 8. Define and test a procedure append that takes two lists, ls1 and ls2, and appends ls1 to ls2.
(define append
  (lambda (a b)
    (cond
      ((null? a) b)
      (else (cons (car a) (append (cdr a) b))))))
(append '(a b c) '(1 2 3))

; 9. Define and test a procedure reverse that takes a list and returns the reverse of that list.
(define reverse
  (lambda (a)
    (letrec
        ((helper (lambda (a stack)
                   (cond
                     ((null? a) stack)
                     (else (helper (cdr a) (cons (car a) stack)))))))
      (helper a '()))))
(reverse '(a 3 x))

; 10. Define and test a procedure fact that takes a natural number and computes the factorial of that number. The factorial of a number is computed by multiplying it by the factorial of its predecessor. The factorial of 0 is defined to be 1.
(define fact
  (lambda (n)
    (letrec
        ((helper (lambda (n result)       
                   (cond
                     ((zero? n) result)
                     (else (helper (- n 1) (* n result)))))))
      (helper n 1))))
(fact 0)
(fact 5)

; 11. Define and test a procedure memv that takes an element and a list and returns the first cdr whose car is eqv? to the element, or #f if the element is absent from the list.
(define memv
  (lambda (a b)
    (cond
      ((null? b) #f)
      ((eqv? (car b) a) b)
      (else (memv a (cdr b))))))
(memv 'a '(a b c))
(memv 'b '(a ? c))
(memv 'b '(a b c b))

; 12. Define and test a procedure fib that takes a natural number n as input and computes the nth number, starting from zero, in the Fibonacci sequence (0, 1, 1, 2, 3, 5, 8, 13, 21, …). Each number in the sequence is computed by adding the two previous numbers.
(define fib
  (lambda (n)
    (letrec
        ((helper (lambda (a b n)
                   (cond
                     ((zero? n) a)
                     (else (helper b (+ a b) (sub1 n)))))))
      (helper 0 1 n))))
(fib 0)
(fib 1)
(fib 7)

; 13. The expressions (a b) and (a . (b . ())) are equivalent. Using this knowledge, rewrite the expression ((w x) y (z)) using as many dots as possible. Be sure to test your solution using Racket's equal? predicate. (You do not have to define a rewrite procedure; just rewrite the given expression by hand and place it in a comment.)
(equal? '(a b) '(a . (b . ())))
(equal? '((w x) y (z)) '((w . (x . ())) . (y . ((z . ())))))

; 14. Define and test a procedure binary->natural that takes a flat list of 0s and 1s representing an unsigned binary number in reverse bit order and returns that number.
(define binary->natural
  (lambda (a)
    (letrec
        ((helper (lambda (a pow res)
                   (cond
                     ((null? a) res)
                     ((eqv? (car a) 1) (helper (cdr a) (* 2 pow) (+ pow res)))
                     (else (helper (cdr a) (* 2 pow) res))))))
      (helper a 1 0))))
(binary->natural '())
(binary->natural '(0 0 1))
(binary->natural '(0 0 1 1))

; 15. Define subtraction using natural recursion. Your subtraction function, minus, need only take nonnegative inputs where the result will be nonnegative.
(define minus
  (lambda (x y)
    (cond
      ((zero? y) x)
      (else (minus (sub1 x) (sub1 y))))))
(minus 5 3)
(minus 100 50)

; 16. Define division using natural recursion. Your division function, div, need only work when the second number evenly divides the first. Division by zero is of course bad data.
(define div
  (lambda (x y)
    (cond
      ((zero? x) 0)
      ((+ 1 (div (- x y) y))))))
(div 25 5)
(div 36 6)

; 17. Define a function append-map that, similar to map, takes both a procedure p of one argument a list of inputs ls and applies p to each of the elements of ls. Here, though, we mandate that the result of p on each element of ls is a list, and we append together the intermediate results. Do not use Racket's built-in append-map in your definition.
(define append-map
  (lambda (a b)
    (cond
      ((null? b) '())
      (else (append (a (car b)) (append-map a (cdr b)))))))
(append-map countdown (countdown 5))

; 18. Define a function set-difference that takes two flat sets (lists with no duplicate elements) s1 and s2 and returns a list containing all the elements in s1 that are not in s2.
(define set-difference
  (lambda (a b)
    (letrec
        ((helper (lambda (a b)
                   (cond
                     ((null? b) #t)
                     ((eqv? (car b) a) #f)
                     (else (helper a (cdr b)))))))
      (cond
        ((null? a) '())
        ((helper (car a) b) (cons (car a) (set-difference (cdr a) b)))
        (else (set-difference (cdr a) b))))))
(set-difference '(1 2 3 4 5) '(2 4 6 8))

; 19. In mathematics, the power set of any set S, denoted P(S), is the set of all subsets of S, including the empty set and S itself.
(define powerset
  (lambda (a)
    (cond
      ((null? a) '(()))
      (else (append (map
                     (lambda (x) (cons (car a) x))
                     (powerset (cdr a)))
                    (powerset (cdr a)))))))
(powerset '(3 2 1))
(powerset '())

; 20. The cartesian-product is defined over a list of sets (again simply lists that by our agreed upon convention don't have duplicates). The result is a list of tuples (i.e. lists). Each tuple has in the first position an element of the first set, in the second position an element of the second set, etc. The output list should contains all such combinations. The exact order of your tuples may differ; this is acceptable.
(define cartesian-product
  (lambda (a)
    (letrec
        ((helper (lambda (a b)
                   (cond
                     ((null? a) '())
                     (else (append
                            (map (lambda (x) (cons (car a) (cons x '()))) b)
                            (helper (cdr a) b)))))))
      (helper (car a) (car (cdr a))))))
(cartesian-product '((5 4) (3 2 1)))
(cartesian-product '((7 6 5) (3 2)))
                   
; 21. Rewrite some of the natural-recursive programs from above instead using foldr. That is, the bodies of your definitions should not refer to themselves. The names should be the following:
(define insertR-fr
  (lambda (a b c)
    (foldr (lambda (x result)
             (cond
               ((eq? a x) (cons a (cons b result)))
               (else (cons x result)))) '() c)))
(insertR-fr 'x 'y '(x z z x y x))

(define filter-fr
  (lambda (a b)
    (foldr (lambda (x result)
             (cond
               ((a x) (cons x result))
               (else result))) '() b)))
(filter-fr even? '(1 2 3 4 5 6))

(define map-fr
  (lambda (a b)
    (foldr (lambda (x result)
             (cons (a x) result)) '() b)))
(map-fr add1 '(1 2 3 4))

(define append-fr
  (lambda (a b)
    (foldr cons b a)))
(append-fr '(a b c) '(1 2 3))

(define reverse-fr
  (lambda (a)
    (foldr (lambda (x result)
             (append result (cons x '()))) '() a)))
(reverse-fr '(a 3 x))

(define binary->natural-fr
  (lambda (a)
    (foldr (lambda (x y)
             (+ x (* 2 y))) 0 a)))
(binary->natural-fr '())
(binary->natural-fr '(0 0 1))
(binary->natural-fr '(0 0 1 1))

(define append-map-fr
  (lambda (a b)
    (foldr (lambda (x result)
             (append (a x) result)) '() b)))
(append-map-fr countdown (countdown 5))

(define set-difference-fr
  (lambda (a b)
    (foldr (lambda (x result1)
             (cond
               ((foldr (lambda (y result2)
                      (cond
                        ((eq? y x) #f)
                        (else result2))) #t b) (cons x result1))
               (else result1))) '() a)))
(set-difference-fr '(1 2 3 4 5) '(2 4 6))

(define powerset-fr
  (lambda (a)
    (foldr (lambda (x result1)
             (append (foldr (lambda (y result2)
                              (cons (cons x y) result2)) '() result1) result1)) '(()) a)))
(powerset-fr '(3 2 1))
(powerset-fr '())

(define cartesian-product-fr
  (lambda (a)
    (letrec
        ((helper (lambda (a b)
                   (foldr (lambda (x result1)
                            (append (foldr (lambda (y result2)
                                             (cons (cons x (cons y '())) result2)) '() b) result1)) '() a))))
      (helper (car a) (car (cdr a))))))
(cartesian-product-fr '((5 4) (3 2 1)))

; 22. Consider a function f defined as below
(define collatz
  (letrec
    ((odd-case
       (lambda (recur)
         (lambda (x)
           (cond 
            ((and (positive? x) (odd? x)) (collatz (add1 (* x 3)))) 
            (else (recur x))))))
     (even-case
       (lambda (recur)
         (lambda (x)
           (cond 
            ((and (positive? x) (even? x)) (collatz (/ x 2))) 
            (else (recur x))))))
     (one-case
       (lambda (recur)
         (lambda (x)
           (cond
            ((zero? (sub1 x)) 1)
            (else (recur x))))))
     (base
       (lambda (x)
         (error 'error "Invalid value ~s~n" x))))
    (one-case (odd-case (even-case base)))
    ))
(collatz 12)
(collatz 120)
(collatz 9999)

; 21. A quine is a program whose output is the listings (i.e. source code) of the original program. In Racket, 5 and #t are both quines.
; ((lambda (x) (list x (list 'quote x))) '(lambda (x) (list x (list 'quote x))))
(define quine
  ((lambda (x) (list x (list 'quote x))) '(lambda (x) (list x (list 'quote x)))))

(equal? quine (eval quine))
(equal? quine (eval (eval quine)))
