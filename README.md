## cpser.rkt

- CPS, A-Normalization, the flatten pass of compiler building and so on.

## interp-machine.rkt

- It's a simple C-style interpreter of λ-Calculus with some primitives.
- interpreter -> CPSed -> registerized -> trampolined.
- In some way, it's like an abstract machine (like CEK machine).

## untyped-lambda-typed/racket.rkt

- It's an interpreter of Call-By-Name λ-Calculus written with #lang typed/racket.

## Others

1. `pmatch.scm` ：a simple pattern matcher of Scheme by Oleg Kiselyov.
2. `encoding.scm` : some primitives encoded by λ-terms by Yin Wang.
