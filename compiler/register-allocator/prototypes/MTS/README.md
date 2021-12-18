## Credit

This is a semantics-based, syntax-directed register allocator. Credit for original idea of *model* and static cache replacement goes to Yin Wang and R. Kent Dybvig.
This project is my implementation with some extensions and tests in Racket. My future work is to justify and further mechanize it in a proof assistant like Agda.

## Big Picture

The main idea is to do abstract interpretation on source language, viewing registers as "caches" of frame variables on procedure call stacks. The analogy of *static* registers and *dynamic* caches is the inspiration of static cache replacement algorithm of this method.

## Source Language

**UIL** (name from R. Kent Dybvig's notes)

```
atm := number
       | vars : x, y, f ... 
       | bool : true, false
uop := - | not
bop := + | - | *
       | eq? | < | > | <= | >= 
exp := atm | (fun-ref f)
       | (bop atm atm)
       | (set! x exp)
       | (if exp exp exp)
       | (while exp exp)
       | (begin exp exp ...)
       | (fun-app exp exp ...)

def := (f (x y z ...) exp)
program := (P '() (def def ...))
```

Example:

## Register Sets & Frame Variables

Adjust to most concrete calling conventions.

1. caller-saved registers (8)
```
caller := r0 | r1 | r2 | r3 | r4 | r5 | r6 | r7
```
2. callee-saved registers (5)
```
callee := r8 | r9 | ra | rb | rc
```
3. parameter passing registers (6)
```
params := r0 | r1 | r2 | r3 | r4 | r5
```

For frame variables:
```
fvs := fv.0, fv.1, fv.2 ...
```
The natural number after `.` means the offset to the base pointer of the current stack frame.

## Goal(s) of the game

1. to minimize register-memory traffic.
2. to minimize register-register traffic.

These two goals are different from standard graph coloring algorithms, which aim to minimize the number of **spills**.
**But how does this difference impact metrics like code generation?**

## Rules of the game

1. allocate each variable one register per occurence.
2. two occurences of the same variable may get allocated two different registers, which means live-range splitting on-the-fly [1].
3. ...


## Model as abstraction of regs & fvs


Recall the `env` argument of a call-by-value interpreter: it is the abstraction of variable bindings of current frame; similarly, in register allocation, there is an abstraction of mappings between variable names and registers plus frame locations. We use `struct` to represent models:

```scheme
(struct M (reg-map fv-map regs))
```
A model consists of 3 subparts: a mapping between registers and variables, a mapping between frame locations and variables, a list of available registers in the current "state" of abstract interpretation.

E.g.

```scheme
(M '((x . rdi) (r . rsi)) '() R@)
```

## Features

1. First-In-First-Out (FIFO) static cache replacement policy.
2. Support whileloops, with less accurate approximation for liveness analysis temporarily.



## Known Issues

1. `tak` in the benchmark tests will generate broken x86 code. 


## Future Work

1. Re-implement in Abstract Machine Style.
2. More efficient operations on models (bind, unbind, delete, shuffle, etc.)
3. Improve code for function calls.

## References

[1] Yin Wang and R. Kent Dybvig. Register allocation by model transformer semantics. CoRR, abs/1202.5539, 2012.
