#lang racket

(require "pmatch.rkt")

; Part 1
(define lex
  (lambda (a b)
    (pmatch a
            [`,x (guard (number? x)) `(const ,x)]
            [`,var (guard (symbol? var)) `(var ,(index-of b var))]
            [`(zero? ,x) `(zero? ,(lex x b))]
            [`(sub1 ,x) `(sub1 ,(lex x b))]
            [`(* ,x ,y) `(* ,(lex x b) ,(lex y b))]
            [`(if ,test ,x ,y) `(if ,(lex test b) ,(lex x b) ,(lex y b))]
            [`(let ((,id ,arg)) ,body) `(let ,(lex arg b) ,(lex body (cons id b)))]
            [`(,x ,y) `(,(lex x b) ,(lex y b))]
            [`(lambda (,x) ,y) `(lambda ,(lex y (cons x b)))])))

; Part 2
(define empty-env
  (lambda ()
    '()))

(define extend-env
  (lambda (id arg env)
    (cons (cons id arg) env)))

(define apply-env
  (lambda (env var)
    (cond
      [(assq var env) => cdr]
      [else (error "no var")])))

; closure-fn
(define closure-fn
  (lambda (id body env)
    (lambda (arg)
      (value-of-fn body (extend-env id arg env)))))

(define apply-closure-fn
  (lambda (closure arg)
    (closure arg)))

(define value-of-fn
  (lambda (exp env)
    (pmatch exp
            [`,n (guard (integer? n)) `,n] ;numbers
            [`,boolean (guard (boolean? boolean)) `,boolean] ;booleans
            ;[`(zero? ,x) `,(zero? (value-of-fn x env))] ;zero?
            ;[`(sub1 ,x) `,(sub1 (value-of-fn x env))] ;sub1
            [`zero? zero?]
            [`sub1 sub1]
            [`(* ,x ,y) `,(* (value-of-fn x env) (value-of-fn y env))] ; *
            [`,var (guard (symbol? var)) `,(apply-env env var)] ;variables
            [`(if ,test ,res1 ,res2) `,(cond
                                         ((value-of-fn test env) (value-of-fn res1 env))
                                         (else (value-of-fn res2 env)))] ;if
            [`(lambda (,id) ,body) `,(closure-fn id body env)] ;lambda, closure-fn
            [`(,id ,body) `,(apply-closure-fn (value-of-fn id env) (value-of-fn body env))] ;application, apply-closure-fn
            [`(let ([,id ,arg]) ,body) `,(value-of-fn body (extend-env id (value-of-fn arg env) env))] ; let
            )))

(value-of-fn
 '(let ([x (* 2 3)])
    (let ([x (sub1 x)])
      (* x x)))
 (empty-env))

; closure
(define closure-ds
  (lambda (id body env)
    `(closure ,id ,body ,env)))

(define apply-closure-ds
  (lambda (closure arg)
    (pmatch closure
            [`(closure ,id ,body ,env)
             (value-of-ds body (extend-env id arg env))])))

(define value-of-ds
  (lambda (exp env)
    (pmatch exp
            [`,n (guard (integer? n)) `,n] ;numbers
            [`,boolean (guard (boolean? boolean)) `,boolean] ;booleans
            [`(zero? ,x) (zero? (value-of-ds x env))] ;zero?
            [`(sub1 ,x) (sub1 (value-of-ds x env))] ;sub1
            ; modify sub1 and zero? due to these two functions are not suitable for closure-ds
            [`(* ,x ,y) `,(* (value-of-ds x env) (value-of-ds y env))] ; *
            [`,var (guard (symbol? var)) `,(apply-env env var)] ;variables
            [`(if ,test ,res1 ,res2) `,(cond
                                         ((value-of-ds test env) (value-of-ds res1 env))
                                         (else (value-of-ds res2 env)))] ;if
            [`(lambda (,id) ,body) `,(closure-ds id body env)] ;lambda
            [`(,id ,body) `,(apply-closure-ds (value-of-ds id env) (value-of-ds body env))] ;application
            [`(let ([,id ,arg]) ,body) `,(value-of-ds body (extend-env id (value-of-ds arg env) env))] ; let
            )))
(value-of-ds
 '(let ([y (* 3 4)])
    ((lambda (x) (* x y)) (sub1 6)))
 (empty-env))
(value-of-ds
 '(let ([x (* 2 3)])
    (let ([y (sub1 x)])
      (* x y)))
 (empty-env))

; Part 3

; Wrong version

; from the definition, "let", instead of input all lambda into the application
; (define value-of-dynamic
;   (lambda (exp env)
;     (pmatch exp
;             [`(quote ,v) v]
;             [`,n (guard (integer? n)) n] ;integer
;             [`,boolean (guard (boolean? boolean)) boolean] ;number
;             [`,var (guard (symbol? var)) (value-of-dynamic (apply-env env var) env)]
;             [`(* ,x ,y) (* (value-of-dynamic x env) (value-of-dynamic y env))]
;             [`(cons ,x ,y) (cons (value-of-dynamic x env) (value-of-dynamic y env))]
;             [`(car ,x) (car (value-of-dynamic x env))]
;             [`(cdr ,x) (cdr (value-of-dynamic x env))]
;             [`(zero? ,x) (zero? (value-of-dynamic x env))] ;zero
;             [`(sub1 ,x) (sub1 (value-of-dynamic x env))] ;sub1
;             [`(null? ,x) (null? (value-of-dynamic x env))] ;null
;             [`(if ,test ,x ,y) (if (value-of-dynamic test env) (value-of-dynamic x env) (value-of-dynamic y env))] ;if
;             [`(lambda (,id) ,body) (lambda (arg) (value-of-dynamic body (extend-env id arg env)))]
;             [`(let ([,id ,arg]) ,body) `,(value-of-dynamic body (extend-env id arg env))]
;             [`(,id ,body) ((value-of-dynamic id env) (value-of-dynamic body env))]
;             [`,ls (guard (list? ls)) ls] ;list
;             )))

(define value-of-dynamic
  (lambda (exp env)
    (pmatch exp
            [`(quote ,v) v]
            [`,n (guard (integer? n)) n] ;integer
            [`,boolean (guard (boolean? boolean)) boolean] ;number
            [`,var (guard (symbol? var)) (apply-env env var)]
            [`(* ,x ,y) (* (value-of-dynamic x env) (value-of-dynamic y env))]
            [`(cons ,x ,y) (cons (value-of-dynamic x env) (value-of-dynamic y env))]
            [`(car ,x) (car (value-of-dynamic x env))]
            [`(cdr ,x) (cdr (value-of-dynamic x env))]
            [`(zero? ,x) (zero? (value-of-dynamic x env))] ;zero
            [`(sub1 ,x) (sub1 (value-of-dynamic x env))] ;sub1
            [`(null? ,x) (null? (value-of-dynamic x env))] ;null
            [`(if ,test ,x ,y) (if (value-of-dynamic test env) (value-of-dynamic x env) (value-of-dynamic y env))] ;if
            [`(lambda (,id) ,body) `(lambda (,id) ,body)]
            [`(let ([,id ,arg]) ,body) `,(value-of-dynamic body (extend-env id (value-of-dynamic arg env) env))]
            [`(,id ,body) (match-let
                              ([`(lambda (,a) ,b) (value-of-dynamic id env)]
                               [`,exp (value-of-dynamic body env)])
                            (value-of-dynamic b (extend-env a exp env)))]
            )))

(value-of-dynamic '(let ([x 2])
                     (let ([f (lambda (e) x)])
                       (let ([x 5])
                         (f 0))))
                  (empty-env))
(value-of-dynamic
 '(let ([! (lambda (n)
             (if (zero? n) 
                 1
                 (* n (! (sub1 n)))))])
    (! 5))
 (empty-env))
(value-of-dynamic
 '(let ([f (lambda (x) (cons x l))])
    (let ([cmap 
           (lambda (f)
             (lambda (l)               
               (if (null? l) 
                   '()
                   (cons (f (car l)) ((cmap f) (cdr l))))))])
      ((cmap f) (cons 1 (cons 2 (cons 3 '())))))) 
 (empty-env))

(define value-of-ri
  (lambda (empty-env-ri extend-env-ri apply-env-ri closure-ri apply-closure-ri)
    (lambda (exp)
      (letrec ((helper (lambda (exp env)
                         (pmatch exp
                                 [`,n (guard (integer? n)) `,n] ;numbers
                                 [`,boolean (guard (boolean? boolean)) `,boolean] ;booleans
                                 [`(zero? ,x) (zero? (helper x env))] ;zero?
                                 [`(sub1 ,x) (sub1 (helper x env))] ;sub1
                                 [`(* ,x ,y) `,(* (helper x env) (helper y env))] ; *
                                 [`,var (guard (symbol? var)) `,(apply-env-ri env var)] ;variables
                                 [`(if ,test ,res1 ,res2) `,(cond
                                                              ((helper test env) (helper res1 env))
                                                              (else (helper res2 env)))] ;if
                                 [`(lambda (,id) ,body) `,(closure-ri id body env)] ;lambda
                                 [`(,id ,body) `,(apply-closure-ri (helper id env) (helper body env))] ;application
                                 [`(let ([,id ,arg]) ,body) `,(helper body (extend-env id (helper arg env) env))] ; let
                                 ))))
        (helper exp (empty-env-ri)))
        )))

; functional (higher-order) representation of environments
(define empty-env-fn
  (lambda ()
    (lambda (y) (error 'value-of "unbound variable ~s" y))))

(define extend-env-fn
  (lambda (id arg env)
    (lambda (var)
      (if (eq? id var)
          arg
          (apply-env-fn env var)))))

(define apply-env-fn
  (lambda (env var)
    (env var)))

; data-structural representation of environments
(define empty-env-ds
  (lambda ()
    `(empty-env-ds)))

(define extend-env-ds
  (lambda (id arg env)
    `(extend-env-ds ,id ,arg ,env)))

(define apply-env-ds
  (lambda (env var)
    (pmatch env
            [`(empty-env-ds) (lambda (y) (error 'value-of "unbound variable ~s" y))]
            [`(extend-env-ds ,id ,arg ,env)
             (if (eq? id var)
                 arg
                 (apply-env-ds env var))])))

; closure-fn
(define closure-fn-ri
  (lambda (id body env)
    (lambda (arg)
      (value-of-fn body (extend-env id arg env)))))

(define apply-closure-fn-ri
  (lambda (closure arg)
    (closure arg)))

; closure
(define closure-ds-ri
  (lambda (id body env)
    `(closure ,id ,body ,env)))

(define apply-closure-ds-ri
  (lambda (closure arg)
    (pmatch closure
            [`(closure ,id ,body ,env)
             (value-of-ds body (extend-env id arg env))])))

((value-of-ri empty-env-fn extend-env-fn apply-env-fn closure-fn-ri apply-closure-fn-ri) '((lambda (x) x) 5))
((value-of-ri empty-env-ds extend-env-ds apply-env-ds closure-ds-ri apply-closure-ds-ri) '((lambda (x) x) 5))
((value-of-ri empty-env-fn extend-env-fn apply-env-fn closure-ds-ri apply-closure-ds-ri) '((lambda (x) x) 5))
((value-of-ri empty-env-ds extend-env-ds apply-env-ds closure-fn-ri apply-closure-fn-ri) '((lambda (x) x) 5))
