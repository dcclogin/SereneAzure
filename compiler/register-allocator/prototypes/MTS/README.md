## Source Language

> under construction

UIL

```
atm := number
       | vars 'x 'y 'z ... 
       | bool 'true 'false
uop := - | not
bop := + | - | *
       | eq? | < | > | <= | >= 
exp := atm | (fun-ref vars)
       | (bop atm atm)
       | (set! x exp)
       | (if exp exp exp)
       | (while exp exp)
       | (begin exp exp ...)
       | (fun-app exp exp ...)
```


## Features

1. First-In-First-Out (FIFO) static cache replacement policy.
2. Support whileloops, with less accurate approximation for liveness analysis temporarily.


## Future Work

1. Re-implement in Abstract Machine Style.
2. More efficient operations on models (bind, unbind, delete, shuffle, etc.)
3. Improve code for function calls.
