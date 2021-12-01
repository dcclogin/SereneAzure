## Source Language

> under construction

UIL

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


## Features

1. First-In-First-Out (FIFO) static cache replacement policy.
2. Support whileloops, with less accurate approximation for liveness analysis temporarily.


## Future Work

1. Re-implement in Abstract Machine Style.
2. More efficient operations on models (bind, unbind, delete, shuffle, etc.)
3. Improve code for function calls.
