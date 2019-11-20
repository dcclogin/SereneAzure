#lang plai-typed
; "AST" for  *B*
(define-type Bexpr
  [t]
  [f]
  [dot (l : Bexpr) (r : Bexpr)])

; "final answer" for *B*
(define-type Bvalue
  [T]
  [F])

; the reduction algorithm
(define (evalB [e : Bexpr]) : Bvalue
  (type-case Bexpr e
    [t () (T)]
    [f () (F)]
    [dot (l r)
         (or* (evalB l)
              (evalB r))] ; compatible
    ))

; the semantics of 'or'
(define (or* [b1 : Bvalue] [b2 : Bvalue]) : Bvalue
  (cond
    [(T? b1) (T)]
    [else
     (cond
       [(T? b2) (T)]
       [else (F)])]))

; the semantics of 'and' 
(define (and* [b1 : Bvalue] [b2 : Bvalue]) : Bvalue
  (cond
    [(F? b1) (F)]
    [else
     (cond
       [(F? b2) (F)]
       [else (T)])]))


; a simple parser from s-exp to AST of *B*

