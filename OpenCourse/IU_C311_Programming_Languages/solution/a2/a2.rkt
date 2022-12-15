#lang racket

(require "pmatch.rkt")
; (require "a2-student-tests.rkt")
; (test-file #:file-name "a2.rkt")

; C311 - 2016?
; Assignment2
; Date: 9th, Dec, 2022
; Writer: Jingyi Yu

; Part 1: Natural Recursion Refresher

; 1. Consider the following partial definition of the list-ref function. It is intended to operate similarly to Racket's list-ref.
(define list-ref
  (lambda (ls n)
    (letrec
        ((nth-cdr
          (lambda (n)
            ;; complete the definition
            (cond
              ((zero? n) ls)
              (else (cdr (nth-cdr (- n 1)))))
            )))
      (car (nth-cdr n)))))

(list-ref '(a b c) 2)
(list-ref '(a b c) 0)

; 2. Define and test a procedure union that takes two lists with no duplicates, and returns a list containing the union of the two input lists. You may find it helpful to use Racket's memv for this definition. Again, the order of the elements in your answer does not matter.

(define union
  (lambda (a b)
    (cond
      ((null? a) b)
      ((list? (memv (car a) b)) (union (cdr a) b))
      (else (cons (car a) (union (cdr a) b))))))
(union '() '())
(union '() '())
(union '(x) '())
(union '(x) '(x))
(union '(x y) '(x z))

; 3. Define and test a procedure extend that takes two arguments, say x and pred. The second argument pred is a predicate. (Recall what predicates are and how to use them from the previous assignment.) What extend returns should be another predicate. The returned predicate should be satisfied exactly by those things that are eqv? to x or satisfy pred.
(define extend
  (lambda (x pred)
    (lambda (y)
      (or (pred y) (eqv? x y)))))
((extend 1 even?) 0)
((extend 1 even?) 1)
((extend 1 even?) 2)
((extend 1 even?) 3)
(filter (extend 1 even?) '(0 1 2 3 4 5))
(filter (extend 3 (extend 1 even?)) '(0 1 2 3 4 5))
(filter (extend 7 (extend 3 (extend 1 even?))) '(0 1 2 3 4 5))

; 4. Define and test a procedure walk-symbol that takes a symbol x and an association list s. An association list is a list of pairs of associated values. Your procedure should search through s for the value associated with x. If the associated value is a symbol, it too must be walked in s. If x has no association, then walk-symbol should return x.
(define walk-symbol
  (lambda (x s)
    (cond
      ((not (assv x s)) x)
      (else (walk-symbol (cdr (assv x s)) s)))))
(walk-symbol 'a '((a . 5)))
(walk-symbol 'a '((b . c) (a . b)))
(walk-symbol 'a '((a . 5) (b . 6) (c . a)))
(walk-symbol 'c '((a . 5) (b . (a . c)) (c . a)))
(walk-symbol 'b '((a . 5) (b . ((c . a))) (c . a)))
(walk-symbol 'd '((a . 5) (b . (1 2)) (c . a) (e . c) (d . e)))
(walk-symbol 'd '((a . 5) (b . 6) (c . f) (e . c) (d . e)))

; Part 2: Free, Bound, Lexical Address
; 5. Define and test a procedure lambda->lumbda that takes a lambda-calculus expression and returns the expression unchanged with the exception that each lambda as a keyword has been replaced with the word lumbda (notice occurrences of lambda as a variable should be left alone).
(define lambda->lumbda
  (lambda (a)
    (pmatch a
            [`,x (guard (not (pair? x))) x]
            [`(,x ,y) `(,(lambda->lumbda x) ,(lambda->lumbda y))]
            [`(lambda (,x) ,y) `(lumbda (,(lambda->lumbda x)) ,(lambda->lumbda y))])))
(lambda->lumbda 'x)
(lambda->lumbda '(lambda (x) x))
(lambda->lumbda '(lambda (z) ((lambda (y) (a z)) (h (lambda (x) (h a))))))
(lambda->lumbda '(lambda (lambda) lambda)) 
(lambda->lumbda '((lambda (lambda) lambda) (lambda (y) y)))
(lambda->lumbda '((lambda (x) x) (lambda (x) x)))

; 6. Define and test a procedure var-occurs? that takes a variable name and a lambda-calculus expression and returns a boolean answering whether that variable occurs in the expression. Here and forevermore in this class we use the word occur in its technical sense: for us, a formal parameter does not count as a variable occurrence.
(define var-occurs?
  (lambda (a b)
    (pmatch b
            [`,x (guard (not (pair? x))) (eq? x a)]
            [`(,x ,y) `,(or (var-occurs? a x) (var-occurs? a y))]
            [`(lambda (,x) ,y) `,(var-occurs? a y)])))
(var-occurs? 'x 'x)
(var-occurs? 'x '(lambda (x) y))
(var-occurs? 'x '(lambda (y) x))
(var-occurs? 'x '((z y) x))
(var-occurs? 'x '(lambda ((lambda (x) y)) x))

; 7. Define and test a procedure vars that takes a lambda-calculus expression and returns a list containing all variables that occur in the expression. This should be a straightforward modification of lambda->lumbda, and the order of the variables in your answer does not matter.
(define vars
  (lambda (a)
    (pmatch a
            [`,x (guard (not (pair? x))) `(,x)]
            [`(,x ,y) `,(append (vars x) (vars y))]
            [`(lambda (,x) ,y) `,(vars y)])))
(vars 'x)
(vars '(lambda (x) x))
(vars '((lambda (y) (x x)) (x y)))
(vars '(lambda (z) ((lambda (y) (a z)) (h (lambda (x) (h a))))))

; 8. Define and test a modification of vars called unique-vars that behaves like vars but does not return duplicates. Use union in your definition.
(define unique-vars
  (lambda (a)
    (pmatch a
            [`,x (guard (not (pair? x))) `(,x)]
            [`(,x ,y) `,(union (unique-vars x) (unique-vars y))]
            [`(lambda (,x) ,y) `,(unique-vars y)])))
(unique-vars '((lambda (y) (x x)) (x y)))
(unique-vars '((lambda (z) (lambda (y) (z y))) x))
(unique-vars '((lambda (a) (a b)) ((lambda (c) (a c)) (b a))))

; 9. Define and test a procedure var-occurs-free? that takes a symbol and a lambda-calculus expression and returns #t if that variable occurs free in that expression, and #f otherwise. The solution developed in class used a list as an accumulator, your solution should not.
(define var-occurs-free?
  (lambda (a b)
    (pmatch b
            [`,x (guard (not (pair? x))) `,(eq? a x)]
            [`(,x ,y) `,(or (var-occurs-free? a x) (var-occurs-free? a y))]
            [`(lambda (,x) ,y) `,(and (not (eq? a x)) (var-occurs-free? a y))])))
(var-occurs-free? 'x 'x)
(var-occurs-free? 'x '(lambda (y) y))
(var-occurs-free? 'x '(lambda (x) (x y)))
(var-occurs-free? 'x '(lambda (x) (lambda (x) x))) 
(var-occurs-free? 'y '(lambda (x) (x y)))
(var-occurs-free? 'y '((lambda (y) (x y)) (lambda (x) (x y))))
(var-occurs-free? 'x '((lambda (x) (x x)) (x x)))

; 10. Define and test a procedure var-occurs-bound? that takes a symbol and a lambda-calculus expression and returns #t if that variable occurs bound in the expression, and #f otherwise. The solution developed in class used an accumulator, your solution should not.
(define var-occurs-bound?
  (lambda (a b)
    (pmatch b
            [`,x (guard (not (pair? x))) #f]
            [`(,x ,y) `,(or (var-occurs-bound? a x) (var-occurs-bound? a y))]
            [`(lambda (,x) ,y) `,(or (and (eq? a x) (var-occurs-free? a y)) (var-occurs-bound? a y))])))
(var-occurs-bound? 'x 'x)
(var-occurs-bound? 'x '(lambda (x) x))
(var-occurs-bound? 'y '(lambda (x) x))
(var-occurs-bound? 'x '((lambda (x) (x x)) (x x)))
(var-occurs-bound? 'z '(lambda (y) (lambda (x) (y z))))
(var-occurs-bound? 'z '(lambda (y) (lambda (z) (y z))))
(var-occurs-bound? 'x '(lambda (x) y))
(var-occurs-bound? 'x '(lambda (x) (lambda (x) x)))

; 11. Define and test a procedure unique-free-vars that takes a lambda-calculus expression and returns a list of all the variables that occur free in that expression. Order doesn't matter, but the list must not contain duplicate variables. You may find it helpful to use the definition of unique-vars as a starting point.
(define unique-free-vars
  (lambda (a)
    (pmatch a
            [`,x (guard (not (pair? x))) `(,x)]
            [`(,x ,y) `,(union (unique-free-vars x) (unique-free-vars y))]
            [`(lambda (,x) ,y) `,(remv x (unique-free-vars y))])))

(unique-free-vars 'x)
(unique-free-vars '(lambda (x) (x y)))
(unique-free-vars '((lambda (x) ((x y) e)) (lambda (c) (x (lambda (x) (x (e c)))))))

; 12. Define and test a procedure unique-bound-vars that takes a lambda-calculus expression and returns a list of all the variables that occur bound in the input expression. Order doesn't matter, but the list must not contain duplicate variables.
(define unique-bound-vars
  (lambda (a)
    (pmatch a
            [`,x (guard (not (pair? x))) `()]
            [`(,x ,y) `,(union (unique-bound-vars x) (unique-bound-vars y))]
            [`(lambda (,x) ,y) `,(union (unique-bound-vars y) (filter (lambda (t) (eq? t x)) (unique-free-vars y)))])))
(unique-bound-vars 'x)
(unique-bound-vars '(lambda (x) y))
(unique-bound-vars '(lambda (x) (x y)))
(unique-bound-vars '((lambda (x) ((x y) e)) (lambda (c) (x (lambda (x) (x (e c)))))))
(unique-bound-vars '(lambda (y) y))
(unique-bound-vars '(lambda (x) (y z)))

; 13. In a subset of Racket where lambdas have only one argument, the lexical address of a variable is the number of lambdas between the place where the variable is bound (also known as the formal parameter) and the place where it occurs. For example, in the following expression:
(define lex
  (lambda (a b)
    (pmatch a
            [`,x (guard (not (pair? x)))
                 `,(cond
                     ((index-of b x) `(var ,(index-of b x)))
                     (else '()))]
            [`(,x ,y) `(,(lex x b) ,(lex y b))]
            [`(lambda (,x) ,y) `(lambda ,(lex y (cons x b)))])))
(lex '(lambda (x) x) '())
(lex '(lambda (y) (lambda (x) y)) '())
(lex '(lambda (y) (lambda (x) (x y))) '())
(lex '(lambda (x) (lambda (x) (x x))) '())
(lex '(lambda (y) ((lambda (x) (x y)) (lambda (c) (lambda (d) (y c))))) '()) 
(lex '(lambda (a)
        (lambda (b)
          (lambda (c)
            (lambda (a)
              (lambda (b)
                (lambda (d)
                  (lambda (a)
                    (lambda (e)
                      (((((a b) c) d) e) a))))))))) '())
(lex '(lambda (a)
        (lambda (b)
          (lambda (c)
            (lambda (w)
              (lambda (x)
                (lambda (y)
                  ((lambda (a)
                     (lambda (b)
                       (lambda (c)
                         (((((a b) c) w) x) y))))
                   (lambda (w)
                     (lambda (x)
                       (lambda (y)
                         (((((a b) c) w) x) y))))))))))) '())

; 14. Consider again the scenario of the walk-symbol problem. Imagine that we frequently look up values in that association list. Walking the full chain every time may become prohibitively expensive, as certain perverse chains may be arbitrarily long. Consider the work you would have to do to walk a twice in the following association list.
(define walk-symbol-update
  (lambda (x s)
      (let
          ((tmp (assv x s)))
           (cond
             ((not tmp) x)
             (else (let
                       ((res (walk-symbol-update (unbox (cdr tmp)) s)))
                     (set-box! (cdr tmp) res)
                     res))))))

(define a-list `((c . ,(box 15)) (e . ,(box 'f)) (b . ,(box 'c)) (a . ,(box 'b))))
a-list
(walk-symbol-update 'a a-list)
a-list
(walk-symbol-update 'a a-list)
a-list

; 15. A variable can both occur free and occur bound in the same expression. Define a predicate var-occurs-both? that takes a variable x and a lambda-calculus expression, and returns two values, the first of which is a boolean answering whether the variable occurs free in the expression, and the second is a boolean answering whether the var occurs bound in the expression. Your solution should be a one-pass solution, meaning you should not recur over the same data twice, and you should not use an accumulator. In order to return multiple values, you should see the Racket documentation on values, let-values, and call-with-values.
(define var-occurs-both?
  (lambda (a b)
    (pmatch b
            [`,x (guard (not (pair? x))) `,(values (eq? a x) #f)]
            [`(,x ,y) `,(let*-values
                            ([(rx1 rx2) (var-occurs-both? a x)]
                             [(ry1 ry2) (var-occurs-both? a y)])
                          (values (or rx1 ry1) (or rx2 ry2)))]
            [`(lambda (,x) ,y) `,(let*-values
                                   ([(ry1 ry2) (var-occurs-both? a y)])
                                 (values (and (not (eq? a x)) ry1)
                                         (or (and (eq? a x) ry1) ry2)))])))
(var-occurs-both? 'x '(lambda (x) (x (lambda (x) x))))
(var-occurs-both? 'x '(x (lambda (x) x)))
(var-occurs-both? 'x '(lambda (y) (x (lambda (x) x)))) 
(var-occurs-both? 'x '(lambda (x) (lambda (x) (x (lambda (x) x)))))
(var-occurs-both? 'x '(lambda (x) (lambda (y) (lambda (x) (x (lambda (x) x))))))
(var-occurs-both? 'x '(lambda (y) (lambda (x) (lambda (z) (lambda (x) (x (lambda (x) x)))))))