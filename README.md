# bf


## BBC Micro 6502

Thought it would be fun to write a bf interpreter in 6502 to run on my BBC Micro.

First step. Write one in C to remember the details.

Here's the [6502 for my BBC](bbc/bf.6502).


## Experiments in compilation

Hijack this earlier repo of mine to do some experiments in bf compilation.

Spurred on by reading Eli Bendersky's [_Adventures in JIT compilation: Part 1 - an interpreter_](https://eli.thegreenplace.net/2017/adventures-in-jit-compilation-part-1-an-interpreter/),
which to my eyes is entirely AOT (_Ahead of time_) compilation, I want to explore if my idea for static recompilation, developed for
[space-invaders 8080](https://github.com/Nick-Chapman/space-invaders),
will transfer over to brainfuck code, and at what stage does AOC become JIT.

First step: Implement an interpreter (in haskell), written in the _phase-generic_ style, to allow it to be specialized to specific bf programs.


## Rust

Another hijack. Try out some rust coding...

    (cd rust/bf; cargo run --release ../../b/mandelbrot.b)
