#lang racket

(require "pmatch.rkt")

; Part1
(define empty-env
  (lambda ()
    (lambda (var) (error "no var"))))

(define extend-env
  (lambda (id arg env)
    (lambda (var)
      (cond
        ((eq? var id) arg)
        (else (env var))))))

(define apply-env
  (lambda (env var)
    (env var)))

; closure is just a way to `store` body and env
(define closure-cbv
  (lambda (id body env)
    (lambda (arg)
      (val-of-cbv body (extend-env id arg env)))))

(define closure-cbr
  (lambda (id body env)
    (lambda (arg)
      (val-of-cbr body (extend-env id arg env)))))

(define closure-cbname
  (lambda (id body env)
    (lambda (arg)
      (val-of-cbname body (extend-env id arg env)))))

(define closure-cbneed
  (lambda (id body env)
    (lambda (arg)
      (val-of-cbneed body (extend-env id arg env)))))

(define apply-closure
  (lambda (closure arg)
    (closure arg)))

; implementing set! requires to use box
; to distinguish cbv and cbr, the easiest way is to use different box when a parameter is a symbol.
; the situations that paramter is not symbol are not trivial --- if it is not a symbol, it is a value!
(define val-of-cbv
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`,y #:when (symbol? y) (unbox (apply-env env y))]
      [`(zero? ,n) (zero? (val-of-cbv n env))]
      [`(null? ,n) (null? (val-of-cbv n env))]
      [`(sub1 ,n) (sub1 (val-of-cbv n env))]
      [`(add1 ,n) (add1 (val-of-cbv n env))]
      [`(* ,n1 ,n2) (* (val-of-cbv n1 env) (val-of-cbv n2 env))]
      [`(quote ,v) v]
      
      ; cons^, car^, cdr^
      ; cons^ evaluates only when car^ and cdr^
      [`(cons^ ,e ,lst) (cons (box (lambda () (val-of-cbv e env))) (box (lambda () (val-of-cbv lst env))))]
      [`(car^ ,lst) (let* ((bx (car (val-of-cbv lst env)))
                           (value (unbox bx)))
                      (if (procedure? value)
                          (let ((val (value)))
                            (set-box! bx val)
                            val)
                          value))]
      [`(cdr^ ,lst) (let* ((bx (cdr (val-of-cbv lst env)))
                           (value (unbox bx)))
                      (if (procedure? value)
                          (let ((val (value)))
                            (set-box! bx val)
                            val)
                          value))]

      ; cons, car, cdr
      [`(cons ,e ,lst) (cons (val-of-cbv e env) (val-of-cbv lst env))]
      [`(car ,lst) (car (val-of-cbv lst env))]
      [`(cdr ,lst) (cdr (val-of-cbv lst env))]
      
      ; let
      [`(let ((,id ,arg)) ,body) (val-of-cbv body (extend-env id (box (val-of-cbv arg env)) env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbv test env)
                                    (val-of-cbv conseq env)
                                    (val-of-cbv alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbv e1 env) (val-of-cbv e2 env))]
      [`(random ,n) (random (val-of-cbv n env))]
      [`(set! ,e1, e2) (set-box! (apply-env env e1) (val-of-cbv e2 env))]
      [`(lambda (,x) ,body) (closure-cbv x body env)]
      [`(,rator ,rand) (apply-closure (val-of-cbv rator env)
                                      (box (val-of-cbv rand env)))])))

(define val-of-cbr
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`,y #:when (symbol? y) (unbox (apply-env env y))]
      [`(zero? ,n) (zero? (val-of-cbr n env))]
      [`(sub1 ,n) (sub1 (val-of-cbr n env))]
      [`(* ,n1 ,n2) (* (val-of-cbr n1 env) (val-of-cbr n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbr test env)
                                    (val-of-cbr conseq env)
                                    (val-of-cbr alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbr e1 env) (val-of-cbr e2 env))]
      [`(random ,n) (random (val-of-cbr n env))]
      [`(set! ,e1 ,e2) (set-box! (apply-env env e1) (val-of-cbr e2 env))]
      [`(lambda (,x) ,body) (closure-cbr x body env)]
      ; to avoid multiple level boxes
      ; i.e. we can only box not symbol input
      [`(,rator ,rand) #:when (symbol? rand)
                       (apply-closure (val-of-cbr rator env)
                                      (apply-env env rand))]
      [`(,rator ,rand) (apply-closure (val-of-cbr rator env)
                                      (box (val-of-cbr rand env)))])))
; cbname
(define val-of-cbname
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      ; now what the env stores are those symbol binding "lambda()"
      [`,y #:when (symbol? y) ((unbox (apply-env env y)))]
      [`(zero? ,n) (zero? (val-of-cbname n env))]
      [`(sub1 ,n) (sub1 (val-of-cbname n env))]
      [`(* ,n1 ,n2) (* (val-of-cbname n1 env) (val-of-cbname n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbname test env)
                                    (val-of-cbname conseq env)
                                    (val-of-cbname alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbname e1 env) (val-of-cbname e2 env))]
      [`(random ,n) (random (val-of-cbname n env))]
      [`(lambda (,x) ,body) (closure-cbname x body env)]
      ; either symbol, or just value
      [`(,rator ,rand) #:when (symbol? rand)
                       (apply-closure (val-of-cbname rator env)
                                      (apply-env env rand))]
      [`(,rator ,rand) (apply-closure (val-of-cbname rator env)
                                      (box (lambda () (val-of-cbname rand env))))])))

(define unbox/need
  (lambda (b)
    (let ([val ((unbox b))]) ;; (unbox b) return a thunk, ((unbox b)) apply a thunk
      (set-box! b (lambda () val)) ;; update the box to hold out
      val)))

(define val-of-cbneed
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`,y #:when (symbol? y) (unbox/need (apply-env env y))]
      [`(zero? ,n) (zero? (val-of-cbneed n env))]
      [`(sub1 ,n) (sub1 (val-of-cbneed n env))]
      [`(* ,n1 ,n2) (* (val-of-cbneed n1 env) (val-of-cbneed n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbneed test env)
                                    (val-of-cbneed conseq env)
                                    (val-of-cbneed alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbneed e1 env) (val-of-cbneed e2 env))]
      [`(random ,n) (random (val-of-cbneed n env))]
      [`(set! ,e1, e2) (set-box! (apply-env env e1) (val-of-cbneed e2 env))]
      [`(lambda (,x) ,body) (closure-cbneed x body env)]
      [`(,rator ,rand) #:when (symbol? rand)
                       (apply-closure (val-of-cbneed rator env)
                                      (apply-env env rand))]
      [`(,rator ,rand) (apply-closure (val-of-cbneed rator env)
                                      (box (lambda () (val-of-cbneed rand env))))])))

(define cons-test
    '(let ((fix (lambda (f)
                 ((lambda (x) (f (lambda (v) ((x x) v))))
                  (lambda (x) (f (lambda (v) ((x x) v))))))))
        (let ((map (fix (lambda (map)
                          (lambda (f)
                            (lambda (l)
                               (if (null? l)
                                   '()
                                   (cons^ (f (car^ l))
                                          ((map f) (cdr^ l))))))))))
          (let ((take (fix (lambda (take)
                             (lambda (l)
                               (lambda (n)
                                 (if (zero? n)
                                     '()
                                      (cons (car^ l) 
                                            ((take (cdr^ l)) (sub1 n))))))))))
            ((take ((fix (lambda (m)
                           (lambda (i)
                             (cons^ 1 ((map (lambda (x) (add1 x))) (m i)))))) 0)) 5)))))
(val-of-cbv cons-test (empty-env))
