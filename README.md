# OmniLisp

OmniLisp is a multi-target transpiler for a Racket-like syntax. It reads a small
Racket-style source language, lowers it into a target-neutral intermediate
representation, and emits code through pluggable backends.

The current compiler is implemented in Racket. Racket is the host language for
the compiler, while OmniLisp's IR is designed to stay independent of Racket
runtime objects and expander internals. This keeps one frontend pipeline usable
across several output languages.

## Project Status

Built-in targets in this worktree:

- `python` - Python backend with imports, keyword arguments, attributes, and
  collection literals.
- `cpp` - C++20 backend for the neutral IR core subset.
- `rust` - Rust backend for the neutral IR core subset.

The backend architecture is intentionally extensible. The backend guide includes
the registration pattern for additional targets such as JavaScript.

## Repository Layout

- `omnilisp/main.rkt` - command-line entrypoint.
- `omnilisp/parser.rkt` - source reader and surface-form lowering.
- `omnilisp/ir.rkt` - target-neutral IR structs.
- `omnilisp/backends.rkt` - backend registry and target dispatch.
- `omnilisp/*-backend.rkt` - backend code generators.
- `examples/` - sample OmniLisp programs for supported backends.
- `tests/transpiler.rkt` - parser, backend, and native compile smoke tests.
- [BACKEND_GUIDE.md](BACKEND_GUIDE.md) - backend architecture and extension
  guide.
- [Agent.md](Agent.md) - implementation notes and architectural rules.

## Installation

OmniLisp currently runs directly from the repository.

1. Install Racket from <https://racket-lang.org/>.
2. Clone the repository and enter it:

   ```bash
   git clone <repo-url> OmniLisp
   cd OmniLisp
   ```

3. Verify the CLI can see the registered targets:

   ```bash
   racket omnilisp/main.rkt --list-targets
   ```

Optional native toolchains are needed only when compiling generated code:

- Python 3 for running generated Python.
- A C++20 compiler such as `clang++`, `g++`, or `c++` for generated C++.
- `rustc` for generated Rust.
- Node.js for generated JavaScript after a JavaScript backend is registered.

For convenience, you can add a shell alias while working in the repository:

```bash
alias omnilisp='racket omnilisp/main.rkt'
```

## Quick Start

Create a small OmniLisp source file:

```racket
#lang racket

(define (score x)
  (if (> x 10)
      (+ x 2)
      (- x 2)))

(displayln (score 12))
```

Transpile it to Python:

```bash
racket omnilisp/main.rkt -t python score.rkt -o score.py
python3 score.py
```

Inspect the neutral IR before code generation:

```bash
racket omnilisp/main.rkt --dump-ir -t python score.rkt
```

List available targets:

```bash
racket omnilisp/main.rkt --list-targets
```

## Usage

The CLI accepts one input file and writes generated code either to stdout or to
the path provided with `-o`:

```bash
racket omnilisp/main.rkt [--dump-ir] [-t TARGET] [-o OUTPUT] input.rkt
```

If `-t` is omitted, OmniLisp emits Python.

### Python

Python is the default target and supports generic imports, `from` imports,
attribute access, keyword calls, lists, hashes, and common expression forms.

Example source:

```racket
#lang racket

(import (numpy np)
        (:from sklearn.linear_model LinearRegression))

(define x-train
  (py-call np.array
           (list (list 1 1)
                 (list 1 2)
                 (list 2 2)
                 (list 2 3))
           #:dtype "float64"))

(define y-train
  (py-call np.array (list 6 8 9 11) #:dtype "float64"))

(define model (py-call LinearRegression))
(py-call model.fit x-train y-train)
(py-call print model.coef_)
```

Transpile and run:

```bash
racket omnilisp/main.rkt -t python examples/numpy-sklearn-demo.rkt -o demo.py
python3 demo.py
```

### C++

The C++ backend emits C++20 and supports target-specific include and construction
forms that lower into the neutral IR.

Example source:

```racket
#lang racket

(cpp-include <cmath>)

(define (score x)
  (if (> x 10)
      (+ x 2)
      (- x 2)))

(define root (cpp-call (:: std sqrt) 16.0))
(define label (cpp-construct (:: std string) "root"))
(define values (list 1 2 3))

(displayln label)
(displayln root)
(displayln (score 12))
(displayln values)
```

Transpile, compile, and run:

```bash
racket omnilisp/main.rkt -t cpp examples/cpp-demo.rkt -o demo.cpp
c++ -std=c++20 demo.cpp -o demo
./demo
```

### Rust

The Rust backend emits Rust code for the neutral IR core subset and supports
`rust-use` imports.

Example source:

```racket
#lang racket

(rust-use std::f64::consts::PI)

(define (score x)
  (if (> x 10)
      (+ x 2)
      (- x 2)))

(define values (list 1 2 3))
(define radius 2.0)
(define area (* PI radius radius))

(displayln area)
(displayln (score 12))
(displayln values)
```

Transpile, compile, and run:

```bash
racket omnilisp/main.rkt -t rust examples/rust-demo.rkt -o demo.rs
rustc demo.rs -o demo
./demo
```

### JavaScript

JavaScript is not registered as a built-in backend in this worktree. The backend
registry is designed so a JavaScript backend can be added without changing the
frontend or CLI. See [BACKEND_GUIDE.md](BACKEND_GUIDE.md) for the registration
steps and JavaScript backend skeleton.

Once a JavaScript backend is registered under a target such as `javascript`, the
usage follows the same CLI shape:

```bash
racket omnilisp/main.rkt -t javascript input.rkt -o output.js
node output.js
```

## Source Language Notes

The frontend accepts `#lang racket` source and supports a focused Racket-like
subset, including:

- top-level `define` values and functions
- `if`, `when`, `unless`, `cond`, `let`, `lambda`, and `begin`
- function calls with positional and keyword arguments
- `list` and `hash` literals
- imports, attribute access, and scoped references
- backend convenience forms such as `py-call`, `cpp-call`, `cpp-include`, and
  `rust-use`

Surface forms are lowered into OmniLisp's neutral IR before code generation.
Target-specific convenience forms should lower into generic IR nodes instead of
leaking target-only semantics into the IR.

## Development

Run the test suite:

```bash
racket tests/transpiler.rkt
```

The tests cover frontend lowering, backend dispatch, and compile smoke tests for
generated C++ and Rust when the native toolchains are available.

To add another backend, start with [BACKEND_GUIDE.md](BACKEND_GUIDE.md). For
the current architectural direction and non-goals, read [Agent.md](Agent.md).
