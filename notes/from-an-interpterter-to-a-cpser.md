## From an Interpreter to a CPSer

by Chenchao Ding, 2021


Recall that:
- An interpreter is a program that takes an **expression** as input, and gets its **meaning** as output, by **evaluation**.
- An ANFer is a program that takes an **expression** as input, and get its **A-Normal Form** as output, by **correctness preserving transformation**.
- An CPSer is a program that takes an **expression** as input, and get its **CPSed form** as output, by **correctness preserving transformation**.

Somehow you may have an intuitive thought: could we derive a ANFer/CPSer from an interpreter, given the language (grammar, or patterns matched)?

This is a tutorial note that shows you how to **manually** perform such a derivation, or say, a transformation! We first show how to quickly get an ANFer from an interpreter, then from ANFer to CPSer. It's like a "two-pass" derivation.

### Interpreter

First, we should quickly go through the Call-By-Value interpreter of lambda calculus with some primitives:

> under construction (a interpreter written in Racket)

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

- there is a evaluation context, `(+ [ ] (+ 4 2))`, where `[ ]` is a "hole" waiting for something to fill in.
- there is a control expression, i.e. `(* 2 3)`, which gets reduced/evaluated to `6`, then **gives back** the result to the "hole".

This kind of perspective can be better illustrated in a CPSed interpreter, since a evaluation context is essentially a continuation:

> under construction (a CPSed interpreter in Racket)

Now focus on that `v`, which can be read as "the already evaluated value from `(,e1 ,e2)`". It's the right thing to fill in the "hole".



--------------------

Similarly, what is happening when the ANFer meets the same expression `(+ (* 2 3) (+ 4 2))`?

- there is also a context `(+ [ ] (+ 4 2))`, where `[ ]` is a "hole" waiting for something to fill in.
- there is also a control expression, `(* 2 3)`, which is given a name `v.0` to refer to it later, then **gives back** the name to the "hole".

```racket
`(let ([v.0 (* 2 3)])
   ,(anf `(+ v.0 (* 2 3))))
```

You can think as if the ANF transformation "defers" the evaluation to some later steps, or passes, pretty much like a compiler's job. In fact, ANF is an important compiler pass that exposes the "intra-expression" control flow and unnests the complex expressions.

ANF doesn't discriminate between `Number` and `Symbol`, it won't try to **evaluate** variables (i.e. lookup in the `env`).

----------------------


Could we write a program `t` that **automatically** transform an interpreter to a "corresponding" CPSer?
