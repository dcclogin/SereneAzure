#lang typed/racket

; definition of λ-calculus
(define-type LC-Exp (U id abs app))
(struct id ([i : Symbol]))
(struct abs ([i : Symbol] [e : LC-Exp]))
(struct app ([f : LC-Exp] [a : LC-Exp]))

; definition of S-expression
(define-type S-Exp (U Symbol S-List))
(define-type S-List (U Null (Listof S-Exp)))

; Why I need this function ?
(define tosym
  (lambda (s)
    (match s
      [(? symbol? x) x])))

; parser from S-expressions to ASTs of λ
(: parse (-> S-Exp LC-Exp))
(define parse
  (lambda (exp)
    (cond
      [(symbol? exp) (id exp)]
      [(list? exp)
       (match exp
         [`(,e1 ,e2) (app (parse e1) (parse e2))]
         [`(lambda (,x) ,e) (abs (tosym x) (parse e))])])))

; anti-parser from ASTs of λ to S-exp
(: anti-parse (-> LC-Exp S-Exp))
(define anti-parse
  (lambda (lc)
    (match lc
      [(id i) i]
      [(abs i e) `(lambda (,i) ,(anti-parse e))]
      [(app f a) `(,(anti-parse f) ,(anti-parse a))])))

; occurs-free? algorithm
(: occurs-free? (-> Symbol LC-Exp Boolean))
(define occurs-free?
  (lambda (x lc)
    (match lc
      [(id i) (eq? x i)]
      [(abs i e) (and (not (eq? x i))
                      (occurs-free? x e))]
      [(app f a) (or (occurs-free? x f)
                     (occurs-free? x a))])))

; redefine the 'generate symbol' algorithm
(: gensym (-> Symbol Symbol))
(define gensym
  (let ([n -1])
    (lambda (x)
      (set! n (+ 1 n))
      (string->symbol
       (string-append (symbol->string x) "." (number->string n))))))

; substitution algotithm M[y<-N]
(: subst (-> Symbol LC-Exp LC-Exp LC-Exp))
(define subst
  (lambda (y N M)
    (cond
      [(occurs-free? y M)
       (match M
         [(id i) N]
         [(app e1 e2)
          (app (subst y N e1)
               (subst y N e2))]
         [(abs x e)
          (cond
            [(occurs-free? x N)
             (let* ([x* (gensym x)]
                    [e* (subst x (parse x*) e)])
               (if (eq? x* y)
                   (let ([x* (gensym x)])
                     (abs x* (subst y N e*)))
                   (abs x* (subst y N e*))))]
            [else (abs x (subst y N e))])])]
      [else M])))

(: subst-surface (-> Symbol S-Exp S-Exp S-Exp))
(define subst-surface
  (lambda (y N M)
    (anti-parse
     (subst y
            (parse N)
            (parse M)))))


;(subst-surface 'y 'x '(lambda (x) (x y)))
(subst-surface 'x.0 'x '(lambda (x) (x x.0)))
(subst-surface 'x.2 'x '(lambda (x) (x x.2)))
(subst-surface 'y 'x '(lambda (x)
                        (lambda (z)
                          (lambda (x)
                            (y x)))))

; beta reduction (λx.M N) -> M[x<-N]
(: beta (-> LC-Exp LC-Exp))
(define beta
  (lambda (redex)
    (match redex
      [(app f a)
       (match f
         [(abs i e) (subst i a e)]
         [other (error 'beta "not an abstraction.")])]
      [other (error 'beta "not an application.")])))


; one-step-reduction
(: osr (-> LC-Exp LC-Exp))
(define osr
  (lambda (exp)
    (match exp
      [(id i) exp]
      [(abs i e) exp]
      [(app f a)
       (match f
         [(id i0) (error 'osr "not an abstraction.")]
         [(abs i0 e0) (beta exp)]
         [(app f0 a0) (app (osr f) a)])])))

(: is-halt? (-> LC-Exp Boolean))
(define is-halt?
  (lambda (exp)
    (match exp
      [(id i) #t]
      [(abs i e) #t]
      [(app f a) #f])))

; reduction with osr
(: reduce (-> LC-Exp LC-Exp))
(define reduce
  (lambda (exp)
    (cond
      [(is-halt? exp) exp]
      [else (reduce (osr exp))])))

(: reduce0 (-> S-Exp S-Exp))
(define reduce0
  (lambda (exp)
    (anti-parse (reduce (parse exp)))))

; reduction without asking questions each step
