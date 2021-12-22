#lang racket
(require "utilities.rkt")

(define R '(rcx rdx rsi rdi r8 r9 r10 rbx r12 r13 r14))
(define binop '(+ - eq? < > <= >=))

(define (interp-performance-fun fun)
  (define mem-w (box 0))
  (define mem-r (box 0))
  (define reg-w (box 0))
  (define reg-r (box 0))
  (define const (box 0))
  (define (slw s)
    (cond
      [(memq s R) reg-w]
      [(fixnum? s) const]
      [(boolean? s) const]
      [else mem-w]))
  (define (slr s)
    (cond
      [(memq s R) reg-r]
      [(fixnum? s) const]
      [(boolean? s) const]
      [else mem-r]))
  (define (! exp)
    (match exp
      [`(set! ,x ,x) 0]
      [`(set! ,x ,y)
       #:when (or (symbol? y) (fixnum? y) (boolean? y))
       (let ([mx (slw x)]
             [my (slr y)])
         (set-box! mx (add1 (unbox mx)))
         (set-box! my (add1 (unbox my))))]
      [`(set! ,x (fun-ref ,f))
       (let ([mx (slw x)])
         (set-box! mx (add1 (unbox mx)))
         (set-box! mem-r (add1 (unbox mem-r))))]
      [`(set! ,x (call ,f))
       (let ([mx (slw x)]
             [mf (slr f)])
         (set-box! mx (add1 (unbox mx)))
         (set-box! mf (add1 (unbox mf))))]
      [`(set! ,x (,bop ,x ,x))
       #:when (memq bop binop)
       (let ([mxw (slw x)]
             [mxr (slr x)])
         (set-box! mxw (add1 (unbox mxw)))
         (set-box! mxr (add1 (add1 (unbox mxr)))))]
      [`(set! ,x (,bop ,x ,b))
       #:when (memq bop binop)
       (let ([mxw (slw x)]
             [mxr (slr x)]
             [mb (slr b)])
         (set-box! mxw (add1 (unbox mxw)))
         (set-box! mxr (add1 (unbox mxr)))
         (set-box! mb (add1 (unbox mb))))]
      [`(set! ,x (,bop ,a ,x))
       #:when (memq bop binop)
       (let ([mxw (slw x)]
             [mxr (slr x)]
             [ma (slr a)])
         (set-box! mxw (add1 (unbox mxw)))
         (set-box! mxr (add1 (unbox mxr)))
         (set-box! ma (add1 (unbox ma))))]
      [`(set! ,x (,bop ,a ,b))
       #:when (memq bop binop)
       (let ([mxw (slw x)]
             [mxr (slr x)]
             [ma (slr a)]
             [mb (slr b)])
         (set-box! mxw (add1 (add1 (unbox mxw))))
         (set-box! mxr (add1 (unbox mxr)))
         (set-box! ma (add1 (unbox ma)))
         (set-box! mb (add1 (unbox mb))))]
      [`(tail-call ,f)
       (let ([mf (slr f)])
         (set-box! mf (add1 (unbox mf))))]
      [`(,bop ,a ,b)
       #:when (memq bop binop)
       (let ([ma (slr a)]
             [mb (slr b)])
         (set-box! ma (add1 (unbox ma)))
         (set-box! mb (add1 (unbox mb))))]
      [(? symbol? x) (let ([mx (slr x)])
                       (set-box! mx (add1 (unbox mx))))]
      [(? fixnum? n) 0]
      [(? boolean? b) 0]
      [`(if ,cnd ,thn ,els)
       (begin (! cnd) (! thn) (! els) 0)]
      [`(begin . ,s*) (! s*)]
      [`(,s . ,s*) (begin (! s) (! s*) 0)]
      ['() 0]))
  (match fun
    [(Def f args T info exp)
     (! exp)
     (values `(memory-read ,(unbox mem-r))
             `(memory-write ,(unbox mem-w))
             `(register-read ,(unbox reg-r))
             `(register-write ,(unbox reg-w)))]))

(define f interp-performance-fun)
