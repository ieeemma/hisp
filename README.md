# Hisp

Hisp is a Scheme-like Lisp language with an interpreter written in Haskell.

```scheme
(define (prime? n)
    (let [(lower 2)
          (upper (+ 1(round (sqrt n))))]
    (all true?
        (for x (range lower upper)
            (not (zero? (% n x)))))))

(print-line (prime? 456789877))
```

## Features

- Heirarchy of number types (integers, rationals, and reals)
- Mutable struct types
- Macros
- Pattern matching special form
- SRFI-1 style list library

## Limitations

This is a simple tree-walking interpreter, so the performance is roughly an order of magnitude
slower than Python. The code is also very messy, as I wrote this a while ago as coursework.

## Usage

Build the interpreter with `cabal build`. It can be used called directly to use the REPL, or
called with a file name to run.

See examples in `examples/` for more details.
