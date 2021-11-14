## From an Interpreter to a CPSer

by Chenchao Ding, 2021


Recall that:
- An interpreter is a program that takes an **expression** as input, and gets its **meaning** as output, by **evaluation**.
- An CPSer is a program that takes an **expression** as input, and get its **CPSed form** as output, by **correctness preserving transformation**.

Somehow you may have an intuitive thought: could we derive a CPSer from an interpreter, given the language (grammar)?

This is a tutorial note that shows you how to **manually** perform such a derivation, or say, a transformation! We first show how to quickly get an ANFer from an interpreter, then from ANFer to CPSer. It's like a "two-pass" derivation.

### Interpreter

First, we should quickly go through the Call-By-Value interpreter of lambda calculus with some primitives:

> under construction (a interpreter written in Racket)

Try reasoning in your mind what is happening when the interpreter recursively handles expression like `(+ (* 2 3) (- 4 2))`:

> under construction

It **evaluates** the left-most redex `(* 2 3)`, and then **gives back** the result `6`, thus performing a reduction.
This kind of perspective can be better illustrated in a CPSed interpreter, since the continuation literally does the **giving back** job:

> under construction (a CPSed interpreter in Racket)



Similarly, what is happening when the ANFer meets the same expression `(+ (* 2 3) (- 4 2))`?

> under construction

It **gives a name** to the left-most redex `(* 2 3)`, and then **gives back** the name, a similar "reduction".

You can think as if the ANF transformation "defers" the evaluation to some later steps, or passes, pretty much like a compiler's job.
In fact, ANF is an important compiler pass that exposes the "intra-expression" control flow and unnests the complex expressions.



Could we write a program `t` that **automatically** transform an interpreter to a "corresponding" CPSer?
