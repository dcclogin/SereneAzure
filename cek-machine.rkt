#lang racket
; find whether y has free occurrences in exp
(define occurs-free?
  (lambda (y exp)
    (match exp
      ;[(? number? x)  #f]
      ;[(? symbol? x) (eq? y x)]
      [`(lambda (,x) ,e)
       (and
        (not (eq? y x))
        (occurs-free? y e))]
      [`(,rator ,rand)
       (or
        (occurs-free? y rator)
        (occurs-free? y rand))]
      ; '+', '-', '*', '/'
      [`(,op ,e1 ,e2)
       (or
        (occurs-free? y e1)
        (occurs-free? y e2))]
      [`,e (eq? y e)])))

; M{N/y} or M[y<-N],
; substituting N for the free occurrences of y in M
(define subst
  (lambda (y N M)
    (cond
      [(occurs-free? y M)
       (match M
         [(? symbol? x) N]
         [`(lambda (,x) ,e)
          (cond
            ; if x occurs free in N,
            ; it will be 'captured' after substitution!
            ; so we need to 'rename' the bound x in e first
            [(occurs-free? x N)
             (let* ([x* (gensym x)]
                    [e* (subst x x* e)])
               (if (not (eq? x* y))
                   `(lambda (,x*) ,(subst y N e*))
                   (let ([x* (gensym x)])
                     `(lambda (,x*) ,(subst y N e*)))))]
            [else
             `(lambda (,x) ,(subst y N e))])]
         [`(,rator ,rand) `(,(subst y N rator) ,(subst y N rand))]
         ; '+', '-', '*', '/'
         [`(,op ,e1 ,e2) `(,op ,(subst y N e1) ,(subst y N e2))])]
      [else M])))

; generate symbol for replacing
(define gensym
  (let ([n -1])
    (lambda (x)
      (set! n (+ 1 n))
      (string->symbol
       (string-append (symbol->string x) "." (number->string n))))))

; whether the exp is a value?
(define value?
  (lambda (exp)
    (match exp
      [(? symbol? x) #t]
      [(? number? x) #t]
      [`(lambda (,x) ,e) #t]
      [(or '+ '- '* '/ 'add1 'sub1 'iszero) #t]
      [other #f])))

; beta reduction ((λx.M) N) --> M[x<-N]
(define beta
  (lambda (redex)
    (match redex
      [`((lambda (,x) ,e1) ,e2)
       (subst x e2 e1)])))

; beta-v reduction ((λx.M) V) --> M[x<-V]
(define beta-v
  (lambda (redex)
    (match redex
      [`((lambda (,x) ,e1) ,e2)
       (cond
         [(value? e2)
          (subst x e2 e1)])])))

; delta-v reduction
(define delta-v
  (lambda (redex)
    (match redex
      [`(,op ,e1 ,e2)
       (cond
         [(and (number? e1) (number? e2))
          (match op
            ['+ (+ e1 e2)]
            ['- (- e1 e2)]
            ['* (* e1 e2)]
            ['/ (/ e1 e2)])])]
      [`(,op ,e)
       (cond
         [(number? e)
          (match op
            ; (+ 1) = (+ 1)
            ['+ `(+ ,e)]
            ['- `(- ,e)]
            ['* `(* ,e)]
            ['/ `(/ ,e)]
            ['add1 (add1 e)]
            ['sub1 (sub1 e)]
            ['iszero (if (eq? e 0)
                         '(lambda (x) (lambda (y) x))
                         '(lambda (x) (lambda (y) y)))]
            ; ((+ 1) 2) = 3
            [`(,op2 ,e2)
             (match op2
               ['+ (+ e2 e)]
               ['- (- e2 e)]
               ['* (* e2 e)]
               ['/ (/ e2 e)])])])])))

; one-step-v standard reduction
; find redex correctly?
(define osr-v
  (lambda (exp)
    (match exp
      [`(add1 ,e) (delta-v exp)]
      [`(sub1 ,e) (delta-v exp)]
      [`(iszero ,e) (delta-v exp)]
      [`(lambda (,x) ,e) exp]
      [(? number? x) exp]
      [(? symbol? x) exp]
      ; reduction of application
      ; 保证左边是值的特性被实现了
      [`(,rator ,rand)
       (cond
         [(value? rator)
          (cond
            [(value? rand) (beta-v exp)]
            [else `(,rator ,(osr-v rand))])]
         [else `(,(osr-v rator) ,rand)])]
      [`(,op ,e1 ,e2)
       (match op
         [(or '+ '- '* '/)
          (cond
            [(and (value? e1)
                  (value? e2)) (delta-v exp)]
            [(not (value? e1)) `(,op ,(osr-v e1) ,e2)] ;leftmost
            [(not (value? e2)) `(,op ,e1 ,(osr-v e2))])])])))

; representation of something like '(+ 10), '(* 5)...
; (lambda (x) (+ 10 x)
; (lambda (x) (* 5 x)

; naive representation of Environment:
; (x . 5)
; (v . (λk.M . E))
; using a stack?...

; empty Environment
(define env0 '())

; extend Environment
(define ext-env
  (lambda (k v env)
    (cons `(,k . ,v) env)))

; search the Environment for k's value
(define lookup
  (lambda (k env)
    (let ([p (assq k env)])
      (cond
        [(not p) #f]
        [else (cdr p)]))))

; representation of Continuation Code:
; K = ((stop) ...) ...) ...) ...)

; representation of Continuation 'frame':
; `(stop)

; `((k) arg M E)

; `((k) fun +)
; `((k) fun (+ 10))
; `((k) fun (λx.M . E))

; `((k) ret c)
; `((k) ret +)
; `((k) ret (+ 10))
; `((k) ret (λx.M . E))

; using (cdr K) to get 'last' frame.

; 'end' of Continuation?
(define endk?
  (lambda (K)
    (and (value? (caddr K))
         (null? (cdar K)))))

; Continuation “最末尾” 的两个 code
(define 1st-last-code cdr)
(define 2nd-last-code cdar)
; Continuation Code 的 ‘标签’ 和 ‘内容’
(define tag car)
(define content cadr)

; one-step-reduction of CEK machine
; again, it's fragile & ugly
; C = Control String
; E = Environment
; K = Continuation
(define osr-cek
  (lambda (C E K)
    (cond
      [(and (null? C)
            (null? E))
       (cond
         [(endk? K) #f]
         [else
          (let ([ein (1st-last-code K)]
                [zwei (2nd-last-code K)]
                [drei (caar K)])
            (cond
              ; cek5 focus on the 'arg' left behind before
              ;      and since 'ret' a value following 'arg'
              ;      it has to be a 'fun'. (why?)
              [(and (eq? 'arg (tag zwei))
                    (eq? 'ret (tag ein)))
               (let ([new-C (content zwei)]
                     [new-E (caddr zwei)]
                     [new-K (cons drei `(fun ,(content ein)))])
                 (list new-C new-E new-K))]
              ; cek6 beta-v (with env instead of substitution)
              ;      heavily rely on the order of pattern matching!
              ;      & the condition is really stupid...
              [(and (eq? 'fun (tag zwei))
                    (eq? 'ret (tag ein))
                    (list? (content zwei))
                    (not (eq? (car (content zwei)) '+))
                    (not (eq? (car (content zwei)) '-))
                    (not (eq? (car (content zwei)) '*))
                    (not (eq? (car (content zwei)) '/)))
               (match zwei
                 [`(fun ((lambda (,x) ,M) . ,E))
                  (let ([new-C M]
                        [new-E (ext-env x (content ein) E)]
                        [new-K drei])
                    (list new-C new-E new-K))])]
              ; cek7 delta-v
              [(and (eq? 'fun (tag zwei))
                    (eq? 'ret (tag ein))
                    ;(not (list? (content zwei)))
                    (or (value? (content zwei))
                        (value? (car (content zwei))))
                    (number? (content ein)))
               (let ([new-K (cons
                             drei
                             `(ret ,(delta-v
                                     (cons
                                      (content zwei)
                                      (cdr ein)))))])
                 (list C E new-K))]))])]
      [else
       (match C
         ; cek3 primitive
         ;      again, the order...
         [(or '+ '- '* '/)
          (let ([new-C '()]
                [new-E '()]
                [new-K (cons
                        K
                        `(ret ,C))])
            (list new-C new-E new-K))]
         ; cek1 simply lookup C in E, and 'ret' the value
         [(? symbol? x)
          (let ([new-C '()]
                [new-E '()]
                [new-K (cons
                        K
                        `(ret ,(lookup C E)))])
            (list new-C new-E new-K))]
         ; cek2
         [`(lambda (,x) ,M)
          (let ([new-C '()]
                [new-E '()]
                [new-K (cons
                        K
                        `(ret (,C . ,E)))])
            (list new-C new-E new-K))]
         ; cek3 constant number
         [(? number? x)
          (let ([new-C '()]
                [new-E '()]
                [new-K (cons
                        K
                        `(ret ,C))])
            (list new-C new-E new-K))]
         ; cek4 application
         [`(,M ,N)
          (let ([new-C M]
                [new-K (cons
                        K
                        `(arg ,N ,E))])
            (list new-C E new-K))]
         ; cek4 something like (+ 1 2)
         [`(,op ,e1 ,e2)
          (match op
            [(or '+ '- '* '/)
             (let ([M `(,op ,e1)]
                   [N e2])
               (let ([new-C M]
                     [new-K (cons
                             K
                             `(arg ,N ,E))])
                 (list new-C E new-K)))])])])))

; CEK machine
(define cek
  (let ([n 0])
    (lambda (C E K)
      (let ([res (osr-cek C E K)])
        (if (eq? res #f)
            (begin
              (display "final answer: ")
              (set! n 0)
              (list C E K))
            (begin
              (set! n (+ 1 n))
              (display n)
              (newline)
              (display "-- C: ")
              (display (car res))
              (newline)
              (display "-- E: ")
              (display (cadr res))
              (newline)
              (display "-- K: ")
              (display (caddr res))
              (newline)
              (newline)
              (cek (car res)
                   (cadr res)
                   (caddr res))))))))

;-------------------------------------------------------------------;
;                                tests                              ;
;-------------------------------------------------------------------;

(cek
 '((lambda (k) (* 10 (k 5))) (lambda (u) u))
 '()
 '(stop))

#|
(cek
 '((lambda (x)
     (+ x
        ((lambda (x)
           x)
         2)))
   1)
 '()
 '(stop))

(cek
 '((lambda (x)
     (+ ((lambda (x)
           x)
         2)
        x))
   1)
 '()
 '(stop))
|#

; scoping test
(cek
 '((lambda (x)
     ((lambda (f)
        ((lambda (x)
           (f 3))
         4))
      (lambda (y) (* x y))))
   2)
 '()
 '(stop))

; 可以改进的地方
; 1. C、E、K使用非mutable的数据结构，会产生很多辣鸡
; 2. 做出合适的数据抽象，以适应不同C、E、K的数据结构表示
; 3. 依赖模式匹配顺序，更加合理匹配条件
; 4. 在Java或Typed Racket等有类型标注的语言重新实现一遍

; 5. 参考王垠的版本，可以直接模式匹配‘状态空间’，而且可以反向转移。

; 总结和感悟
; 1. 函数调用是个神奇的‘奇点’，新的substution和新的environment扩展都在此发生
; 2. 直觉上，E、K是ctx stack的更精确的表示，E专注于存储‘延迟的替换’，K专注存储‘等处理’和‘已处理’的term，C、E、K三者一确定如何‘走下一步’的规则

; 问题和举一反三
; 1. 