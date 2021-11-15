# From an Interpreter to a CPSer

by Chenchao Ding, 2021


Recall that:
- An interpreter is a program that takes an **expression** as input, and gets its **meaning** as output, by **evaluation**.
- An ANFer is a program that takes an **expression** as input, and get its **A-Normal Form** as output, by **correctness preserving transformation**.
- A CPSer is a program that takes an **expression** as input, and get its **CPSed form** as output, by **correctness preserving transformation**.

Somehow you may have an intuitive thought: could we derive a ANFer/CPSer from an interpreter, given the language (grammar, or patterns matched)?

This is a tutorial that shows you how to **manually** perform such a derivation, or say, a transformation! We first show how to quickly get an ANFer from an interpreter, then from the ANFer to a CPSer. It's like a "two-pass" derivation.

## Interpreter

First, we should quickly go through the Call-By-Value interpreter of lambda calculus with some primitives:

```racket
(define (interp exp)     
  ;; for simplicity, use struct for closure      
  (struct Closure (f env))
  (define !
    (lambda (exp env)
      (match exp
        [(? symbol? x) (lookup x env)]      
        [(? number? x) x]
        [`(lambda (,x) ,e)
         (Closure exp env)]
        [`(,e1 ,e2)
         (let ([v1 (! e1 env)]
               [v2 (! e2 env)])
           (match v1
             [(Closure `(lambda (,x) ,e) env-save)
              (! e (ext-env x v2 env-save))]))]
        [`(,op ,e1 ,e2)
         (let ([v1 (! e1 env)]
               [v2 (! e2 env)])
           (match op
             ['+ (+ v1 v2)]
             ['* (* v1 v2)]))])))
  (define mt-env '())
  (define ext-env
    (lambda (x v env)
      `((,x . ,v) . ,env)))
  (define lookup
    (lambda (x env)
      (let ([p (assv x env)])
        (cond
          [(not p) #f]
          [else (cdr p)]))))
  (! exp mt-env))
```

Try reasoning in your mind what is happening when the interpreter recursively handles expression like `(+ (* 2 3) (+ 4 2))`:

- there is an evaluation context, `(+ [ ] (+ 4 2))`, where `[ ]` is a "hole" waiting for something to fill in.
- there is a control expression, i.e. `(* 2 3)`, which gets reduced/evaluated to `6`, then **gives back** the result to the "hole".

## CPSed interpreter

This kind of perspective can be better illustrated in a CPSed interpreter, since an evaluation context is essentially a continuation:

```racket
(define (interp exp)     
  ;; for simplicity, use struct for closure      
  (struct Closure (f env))
  (define !
    (lambda (exp env C)                               ;; <= C is the evaluation context
      (match exp
        [(? symbol? x) (C (lookup x env))]      
        [(? number? x) (C x)]
        [`(lambda (,x) ,e) (C (Closure exp env))]
        [`(,e1 ,e2)
         (! e1
            (lambda (v1)                              ;; <= it's a "value" to fill the "hole"
              (! e2
                 (lambda (v2)
                   (match v1
                     [(Closure `(lambda (,x) ,e) env-save)
                      (! e (ext-env x v2 env-save)    ;; <= do evaluation
                        (lambda (v)                   ;; <= focus on this v
                          (C v)))])))))]              ;; <= fill the hole with the value v carries 
        [`(,op ,e1 ,e2)
         (! e1
            (lambda (v1)
              (! e2
                 (lambda (v2)
                   (match op
                     ['+ (C (+ v1 v2))]
                     ['* (C (* v1 v2))])))))])))
  (define mt-env '())
  (define ext-env
    (lambda (x v env)
      `((,x . ,v) . ,env)))
  (define lookup
    (lambda (x env)
      (let ([p (assv x env)])
        (cond
          [(not p) #f]
          [else (cdr p)]))))
  (define id (lambda (v) v))
  (! exp mt-env id))
```

Now focus on that `v`, which can be read as "the already evaluated value from `(,e1 ,e2)`". It's the right thing to fill in the "hole".

## ANFer

Similarly, what is happening when the ANFer meets the same expression `(+ (* 2 3) (+ 4 2))`?

- there is also a context `(+ [ ] (+ 4 2))`, where `[ ]` is a "hole" waiting for something to fill in.
- there is also a control expression, `(* 2 3)`, which is given a name `v.0` to refer to it later, then **gives back** the name to the "hole".

```racket
`(let ([v.0 (* 2 3)])
   ,(anf `(+ v.0 (+ 4 2))))
```

You can think as if the ANF transformation "defers" the evaluation to some later steps, or passes, pretty much like a compiler's job. In fact, ANF is an important compiler pass that exposes the "intra-expression" control flow and unnests the complex expressions.

Now the similarity between a CPSed interpreter and an ANFer has been revealed, we can write down the skeleton code for ANFer:

```racket
(define !
    (lambda (exp C)           ;; <= C is the evaluation context
      (match exp
        [(? symbol? x) (C x)]      
        [(? number? x) (C x)]
        [`(lambda (,x) ,e)
         ...                  ;; <= generate a new name?
         ]
        [`(,e1 ,e2)
         (! e1
            (lambda (v1)      ;; <= now it's a name (instead of a value)
              (! e2
                 (lambda (v2)
                   ...        ;; <= generate a new name for `(,v1 ,v2)
                   ...        ;; <= construct a let-binding
                   ...        ;; <= fill the hole with the new name
                   ))))]
        [`(,op ,e1 ,e2)
         (! e1
            (lambda (v1)
              (! e2
                 (lambda (v2)
                   ...        ;; <= similarly, ... 
                   ))))])))
```
Here are the differences:

- ANF doesn't discriminate between `Number` and `Symbol`, it won't try to **evaluate** variables (i.e. lookup in the `env`).
- we no longer need `env`, `mt-env` and `ext-env`, since they're only for **evaluation** of variables, but ANFer never evaluates a name.
- we don't care what `e1` really is, so we no longer need definition of closure and that pattern matching line.
- we are entering a "new world" when ANFing the body `b` of `(lambda (,x) ,b)`, thus a fresh id context is needed.

The very basic idea is to defer the evaluation a little ... turn a dynamic process into a static expression (using quotation & quasiqutation). It's the philosophical side of programming with notions like "dynamic" and "static"...:)

```racket
(define (anf exp)
  (define !
    (lambda (exp C)
      (match exp
        [(? symbol? x) (C x)]      
        [(? number? x) (C x)]
        [`(lambda (,x) ,e)
         (C `(lambda (,x) ,(! e id)))]          ;; <= enter a "new world" via id context 
        [`(,e1 ,e2)
         (! e1
            (lambda (v1)
              (! e2
                 (lambda (v2)
                   (let ([v (gensym 'v)])       ;; <= new name v
                     `(let ([,v (,v1 ,v2)])     ;; <= let-binding via quasi`
                        ,(C v)))))))]           ;; <= fill in v via unquote,
        [`(,op ,e1 ,e2)
         (! e1
            (lambda (v1)
              (! e2
                 (lambda (v2)
                   ;;TODO 
                   1))))])))
  (define id (lambda (v) v))
  (! exp id))
```

Exercise: 
- Try fill the `TODO` part of the ANFer code.
- What if we name each `lambda`? Modify the code.

## CPSer

Motivation: what do you think is the difference  between these two expressions?

```rachet
(let ([v0 (f x)])
  (let ([v1 (g y)])
    (let ([v2 (v0 v1)])
      v2)))
```

```racket
(lambda (k)
  (f x (lambda (v0)
         (g y (lambda (v1)
                (v0 v1 (lambda (v2)
                         (k v2))))))))
```

They are in some way really the same thing! We can simply modify the "construct let-binding" part of code.

But transforming into CPS is not that trivial if `lambda` is in consideration. Try manually CPS-transforming the following expression:

```racket
(lambda (x) ((f x) (g y)))
```
=> (naively)
```racket
(lambda (x k) 
  (f x (lambda (v0)
         (g y (lambda (v1)
                (v0 v1 (lambda (v2)
                         (k v2))))))))
```
=> (optimized tail call)
```racket
(lambda (x k) 
  (f x (lambda (v0)
         (g y (lambda (v1)
                (v0 v1 k)))))))
```

We can no longer "enter a new world" with the `id` context, instead we need a slightly modified context `idk`:

```racket
(define idk (lambda (v) `(k ,v)))
```
which means a continuation `k` bound by the `lambda` (door of the "new world") is waiting for `v` returned by the "new world".

Moreover, primitives like `+` and `*` should not be CPSed since they are not "serious function calls".

```racket
(define (cps exp)     
  (define !
    (lambda (exp C)
      (match exp
        [(? symbol? x) (C x)]      
        [(? number? x) (C x)]
        [`(lambda (,x) ,e)
         (C `(lambda (,x k) ,(! e idk)))]          ;; <= enter a "new world" via idk context 
        [`(,e1 ,e2)
         (! e1
            (lambda (v1)
              (! e2
                 (lambda (v2)
                   (let ([v (gensym 'v)])          ;; <= new name v
                     `(,v1 ,v2 (lambda (,v)        ;; <= construct CPSed call via quasi`
                        ,(C v))))))))]             ;; <= fill in v via unquote,
        [`(,op ,e1 ,e2)
         (! e1
            (lambda (v1)
              (! e2
                 (lambda (v2)
                   ;;TODO 
                   1))))])))
  (define idk (lambda (v) `(k ,v)))
  (define id (lambda (v) v))
  (! exp id))
```

This is really beautiful because we just wrote a CPSer using CPS!


Exercise:
- Try fill the `TODO` part of CPSer.
- Try tail call optimization (Hint: compare the context `C` with `idk`).

Brainteasers:
- Can you write a program `uncps` that transforms back into original style?
- Can you write a program `anf-to-cps` that **automatically** transforms a ANFer to a CPSer? What about `cps-to-anf`?
- Can you write a program `t` that **automatically** transforms an interpreter to a "corresponding" ANFer or CPSer?

-----------------------

## Related Work

Olivier Danvy shows a one-pass transformation into monadic normal form in [ 1 ], which is very similar to my ANFer. Danvy and Filinski's work [ 2 ] has shown a detailed one-pass CPS transformation. Interesting reader can add features like booleans and conditionals to the source language. I also wrote 2 passes from P523 compiler course using similar techniques shown here, namely `explicate-control` and `remove-complex-opera*`, you can find them [here]() and [here].

## References
1. Danvy, O. 2002. A New One-Pass Transformation into Monadic Normal Form. BRICS Report Series. 9, 52 (Dec. 2002). DOI:https://doi.org/10.7146/brics.v9i52.21767
2. Danvy, O., & Filinski, A. (1992). Representing Control: A Study of the CPS Transformation. Mathematical Structures in Computer Science, 2(4), 361-391. doi:10.1017/S0960129500001535
