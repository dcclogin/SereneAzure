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

; M{N/y} or M[y<-N], substituting N for the free occurrences of y in M
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
            ; so we need to 'rename' the bound x in abstraction first
            ; the free occurrs of x in e = the bound x in the abstraction
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

; generate symbol for renaming
(define gensym
  (let ([n -1])
    (lambda (x)
      (set! n (+ 1 n))
      (string->symbol
       (string-append (symbol->string x) "." (number->string n))))))

; whether the exp is a value? (i.e. not an application)
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

; one-step-v reduction
; it's standard now
; and can find redex correctly
(define osr-v
  (lambda (exp)
    (match exp
      [(? number? x) exp]
      [(? symbol? x) exp]
      [`(lambda (,x) ,e) exp]
      [`(,rator ,rand)
       (cond
         [(value? rator)
          (cond
            [(value? rand) (beta-v exp)]
            [else `(,rator ,(osr-v rand))])]
         [else `(,(osr-v rator) ,rand)])]
      ; '+', '-', '*', '/' 
      [`(,op ,e1 ,e2)
       (match op
         [(or '+ '- '* '/)
          (cond
            [(and (value? e1)
                  (value? e2)) (delta-v exp)]
            [(not (value? e1)) `(,op ,(osr-v e1) ,e2)] ;leftmost
            [(not (value? e2)) `(,op ,e1 ,(osr-v e2))])])])))

; call-by-value
(define reduce-v
  (lambda (e)
    (cond
      [(value? e)
       (begin
         (display "final answer: ")
         e)]
      [else
       (begin
         (display "-> ")
         (let ([next (osr-v e)])
           (begin
             (display next)
             (newline)
             (reduce-v next))))])))


#|
  naive stack representation
|#

(define push cons)
(define pop cdr)
(define top car)
(define mt-stack '())
(define empty-stack? null?)

(define get-exp cadr)
(define get-pos car)

; a stack frame is represented as following:
;
; '(rator (lambda (u) u))
; '(rand (lambda (k) (k))
; ...

; one-step-reduction (call-by-value) with stack
; but it's really fragile and ugly...not even a toy!
(define osr-s
  (lambda (exp stack)
    (match exp
      [`(lambda (,x) ,e)
       (cond
         [(empty-stack? stack) (list exp stack)]
         [else
          (let ([e2 (get-exp (top stack))]
                [ps (get-pos (top stack))])
            (cond
              [(value? e2)
               (cond
                 [(eq? ps 'rand)
                  (let ([new-stack
                         (push `(rator ,exp) (pop stack))])
                    (list e2 new-stack))]
                 [(eq? ps 'rator)
                  (let ([new-stack (pop stack)]
                        [new-exp
                         ; beta-v?
                         (osr-v (list e2 exp))])
                    (list new-exp new-stack))])]
              [else
               (cond
                 ; 根据evaluation context定义此处栈中帧exp位置只能是rand
                 ; 因为rator位置的表达式必须已经被求值过了
                 [(eq? ps 'rand)
                  (let ([new-stack
                         (push `(rator ,exp) (pop stack))])
                    (list e2 new-stack))])]))])]
      ; application
      [`(,rator ,rand)
       (let ([new-stack
              (push `(rand ,rand) stack)]
             [new-exp rator])
         (list new-exp new-stack))]
      ; primitive Q: 怎么处理2个参数的情况？
      ; number or symbol
      [(or (? number? x) (? symbol? x))
       (cond
         [(empty-stack? stack) (list exp stack)]
         [else
          (let ([new-stack (pop stack)]
                [new-exp
                 ; beta-v?
                 (osr-v (list (get-exp (top stack)) exp))])
            (list new-exp new-stack))])])))
                     

; reduction with stack
(define reduce-s
  (lambda (e s)
    (if (and (value? e) (empty-stack? s))
        e
        (let ([new-e (car (osr-s e s))]
              [new-s (cadr (osr-s e s))])
          (reduce-s new-e new-s)))))


;-------------------------------------------------------------------;
;                                tests                              ;
;-------------------------------------------------------------------;


(subst 'y 5 '(lambda (x) (x y)))
(subst 'x 5 '(lambda (x) (x y)))
;(subst 'y 'x '(lambda (x) (x y)))
(subst 'x.0 'x '(lambda (x) (x x.0)))
(subst 'x.2 'x '(lambda (x) (x x.2)))
(subst 'y 'x '(lambda (x)
                (lambda (z)
                  (lambda (x)
                    (y x)))))
#|
(beta-v
 '((lambda (x)
     (lambda (y)
       (lambda (x)
         (+ y x)))) 5))
|#
(osr-v
 '((lambda (x) (f x))
   ((lambda (y) (g y)) 5)))

; inside abstraction
(osr-v
 '(lambda (x)
    ((lambda (x) (f x)) 5)))

(reduce-v
 '((lambda (k) (* 10 (k 5))) (lambda (u) u)))

(reduce-v
 '(* 10 (+ 2 (- 3 (/ 6 2)))))

; lexical scoping!
; occurs-free & subst ensure the inner 'x' 'invisible' from 'f'
; Question: can we implement dynamic scoping with substitution?
(reduce-v
   '((lambda (x)
     ((lambda (f)
        ((lambda (x)
           (f 3))
         4))
      (lambda (y) (* x y))))
   2))

#| loop forever!
(reduce-v
 '((lambda (x) (x x))
   (lambda (x) (x x))))
|#

#|
(osr-s
 '(lambda (x) (x 5))
 (list '(rand (lambda (y) y))))

(osr-s
 '(lambda (y) (y))
 (list '(rator (lambda (x) (x 5)))))

(osr-s
 '(lambda (u) u)
 (list '(rand ((lambda (k) k) 5))))

(osr-s
 '((lambda (k) k) 5)
 (list '(rator (lambda (u) u))))

(osr-s
 '(lambda (k) k)
 (list '(rand 5)
       '(rator (lambda (u) u))))

(osr-s
 '5
 (list '(rator (lambda (k) k))
       '(rator (lambda (u) u))))

(osr-s
 'a
 '())

(reduce-s
 '(lambda (u) u)
 (list '(rand ((lambda (k) k) dcclogin))))
|#