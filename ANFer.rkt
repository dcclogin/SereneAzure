#lang racket

(define gensym
  (lambda (split)
    (let ([n -1])
      (lambda (s)
        (set! n (add1 n))
        (string->symbol
         (string-append
          (symbol->string s) split (number->string n)))))))

(define new-var (gensym "."))

define t!
  (lambda (exp)
    (define !
      (lambda (exp C)
        (match exp
          [(? symbol? x) (C x)]
          [`(lambda (,x) ,e)
           (let ([v (new-var 'f)])
             `(let ([,v (lambda (,x) ,(! e id))])
                ,(C v)))]
          [`(,e₀ ,e₁)
           (! e₀
              (lambda (v₀)
                (! e₁
                   (lambda (v₁)
                     (cond
                       [(eqv? C id)
                        (C `(,v₀ ,v₁))]
                       [else (let ([v (new-var 'w)])
                               `(let ([,v (,v₀ ,v₁)])
                                  ,(C v)))])))))])))
    (define id (lambda (x) x))
    (! exp id)))
