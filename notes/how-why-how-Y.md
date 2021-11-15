# How why how Y!

by Chenchao Ding, 2021

This is a tutorial on how to derive the Y combinator on your own. No fixpoint, no domain theory, I've already made it simple.

We have:
- `lambda`!
- `let`-binding

We don't have:
- `letrec`-binding
- `define`


## First Step

Try writing a recursive function, say `fact`, using `let`, naively.

```racket
(let ([f (lambda (n)
           (cond
	     [(zero? n) 1]
	     [else (* n (f (sub1 n)))]))])
  (f 5))
```

You won't be able to run it, since it doesn't pass static checkings. Here `f` is a free variable in the body of the lambda.

## Second Step

Try to bind that free `f`, using `lambda` the ultimate binder!

```racket
(let ([f (lambda (f)
           (lambda (n)
	     (cond
	       [(zero? n) 1]
	       [else (* n (f (sub1 n))))]))])
  (f 5))
```
But wait, now `(f 5)` and `(f (sub1 n))` is nonsense. What instead should we pass to that newly bound `f`?

> (Try thinking about it yourself for a minute...)

Yes, we should pass `f` to `f`, since making `f` "visible to itself" is our purpose.

```racket
(let ([f (lambda (f)
           (lambda (n)
	     (cond
	       [(zero? n) 1]
	       [else (* n ((f f) (sub1 n))))]))])
  ((f f) 5))
```

You can now run it!

## Third Step

We lift the `(f f)` in the body and give it a new lambda binding.



> under construction
> Chinese reader can refer to my article on bilibili
