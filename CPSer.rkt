#lang racket

(define (cps exp)     
  (define !
    (lambda (exp C)
      (match exp
        [(? symbol? x) (C x)]      
        [(? number? x) (C x)]
        [(? boolean? x) (C x)]
        [`(lambda (,x) ,e)
         (C `(lambda (,x k) ,(! e idk)))] 
        [`(,e1 ,e2)
         (! e1
            (lambda (v1)
              (! e2
                 (lambda (v2)
                   (cond
                     [(eqv? C idk) `(,v1 ,v2 k)]
                     [else
                      (let ([v (new-var 'v)])
                        `(,v1 ,v2
                              (lambda (,v) ,(C v))))])))))]
        [`(,op ,e1 ,e2)
         #:when (memq op bin-op)
         (! e1
            (lambda (v1)
              (! e2
                 (lambda (v2)
                   (C `(,op ,v1 ,v2))))))]
        [`(if ,cnd ,thn ,els)
         (! cnd
            (lambda (cnd@)
              `(if ,cnd@ ,(! thn C) ,(! els C))))])))
  (define gensym
    (lambda (split)
      (let ([n -1])
        (lambda (s)
          (set! n (add1 n))
          (string->symbol
           (string-append
            (symbol->string s) split (number->string n)))))))
  (define new-var (gensym "."))
  (define bin-op '(+ - * eq? > < >= <=))
  (define idk (lambda (v) `(k ,v)))
  (define id (lambda (v) v))
  (pretty-print (! exp id)))


(define Y '(lambda (g)
             ((lambda (f)
                (g (lambda (x) ((f f) x))))
              (lambda (f)
                (g (lambda (x) ((f f) x)))))))
(define F '(lambda (f)
             (lambda (n)
               (if (eq? n 0)
                   1
                   (* n (f (- n 1)))))))
(cps F)
;; Y combinator
(cps Y)
;; factorial 
(cps `((,Y ,F) 5))