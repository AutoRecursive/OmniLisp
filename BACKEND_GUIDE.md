# OmniLisp Backend Development Guide

## Overview

OmniLisp uses a pluggable backend architecture that allows you to add new target languages without modifying the core transpiler. Backends are registered in a central registry and dispatched dynamically based on the `--target` flag.

## Architecture

### Backend Registry (`omnilisp/backends.rkt`)

The backend registry is the central dispatch mechanism. It maintains a list of available backends and provides functions to query and select them.

**Key components:**
- `backend` struct: Defines the interface for all backends
- `backend-registry`: List of all registered backends
- `get-backend`: Looks up a backend by target name
- `emit-program`: Dispatches to the appropriate backend emitter

### Backend Interface

Each backend must conform to this interface:

```racket
(struct backend (id description emitter) #:transparent)
```

**Fields:**
- `id` (symbol): Unique identifier for the backend (e.g., `'python`, `'cpp`, `'rust`)
- `description` (string): Human-readable description shown in `--list-targets`
- `emitter` (procedure): Function that takes an IR program and returns generated code as a string

**Emitter signature:**
```racket
(-> program? string?)
```

The emitter receives a normalized IR program (see `omnilisp/ir.rkt` for IR definitions) and must return the complete generated source code as a string.

## Adding a New Backend

### Step 1: Create the Backend Module

Create a new file `omnilisp/<language>-backend.rkt`:

```racket
#lang racket

(require "ir.rkt")

(provide emit-program)

(define (emit-program prog)
  ;; Your code generation logic here
  ;; Return generated code as a string
  ...)
```

### Step 2: Implement the Emitter

Your emitter should handle all IR node types defined in `ir.rkt`:

- `program`: Top-level program container
- `define-function`: Function definitions
- `define-value`: Variable definitions  
- `if-expr`: Conditional expressions
- `var-ref`: Variable references
- `literal`: Literal values (numbers, strings, booleans, void)
- `function-call`: Function calls
- `lambda-expr`: Anonymous functions
- `list-expr`: List literals

**Example skeleton:**

```racket
(define (emit-program prog)
  (match prog
    [(program decls)
     (string-join (map emit-declaration decls) "\n\n")]))

(define (emit-declaration decl)
  (match decl
    [(define-function name params body)
     (format "function ~a(~a) {\n~a\n}"
             name
             (string-join params ", ")
             (emit-body body))]
    [(define-value name expr)
     (format "const ~a = ~a;" name (emit-expr expr))]))

(define (emit-expr expr)
  (match expr
    [(literal val) (emit-literal val)]
    [(var-ref name) name]
    [(if-expr test then else)
     (format "(~a ? ~a : ~a)"
             (emit-expr test)
             (emit-expr then)
             (emit-expr else))]
    ;; ... handle other expression types
    ))
```

### Step 3: Register the Backend

Edit `omnilisp/backends.rkt`:

1. Add a require for your backend module:
```racket
(require (only-in "javascript-backend.rkt" [emit-program emit-javascript-program]))
```

2. Add your backend to the registry:
```racket
(define backend-registry
  (list
   (backend 'python "Python backend..." emit-python-program)
   (backend 'cpp "C++20 backend..." emit-cpp-program)
   (backend 'rust "Rust backend..." emit-rust-program)
   (backend 'javascript "JavaScript ES6 backend" emit-javascript-program)))  ; NEW
```

That's it! No changes to `transpiler.rkt` or `main.rkt` are needed.

### Step 4: Test Your Backend

Add tests to `tests/transpiler.rkt` in the `backend-tests` suite:

```racket
(test-case "javascript target emits ES6 code"
  (define code
    (transpile-string
     "#lang racket\n(define (add x y) (+ x y))\n(displayln (add 1 2))"
     'js-sample
     #:target 'javascript))
  (check-not-false (regexp-match? #rx"function add\\(x, y\\)" code))
  (check-not-false (regexp-match? #rx"console\\.log" code)))
```

Run tests:
```bash
cd ~/Desktop/OmniLisp
racket tests/transpiler.rkt
```

## Usage

Once registered, your backend is immediately available:

```bash
# List all backends
omnilisp --list-targets

# Transpile to your new backend
omnilisp -t javascript input.rkt -o output.js
```

## Best Practices

1. **Handle all IR nodes**: Ensure your emitter handles every IR node type, even if some map to no-ops in your target language.

2. **Preserve semantics**: The generated code should have the same behavior as the source, respecting evaluation order and side effects.

3. **Generate idiomatic code**: Emit code that follows the conventions of your target language (naming, formatting, idioms).

4. **Include necessary imports/headers**: Your emitter should generate a complete, runnable program including any required imports or boilerplate.

5. **Test compilation**: Add a compile test (like the C++ and Rust examples in `compile-tests`) to verify the generated code actually compiles and runs.

6. **Document language-specific features**: If your backend supports special forms (like `py-call` for Python or `cpp-include` for C++), document them in your backend module.

## Example: Existing Backends

Study the existing backends for reference:

- **`python-backend.rkt`**: Full-featured backend with imports, keyword arguments, and Python-specific features
- **`cpp-backend.rkt`**: Statically-typed backend with C++20 features, templates, and includes
- **`rust-backend.rkt`**: Functional backend with Rust's ownership model and type inference

Each demonstrates different approaches to code generation and handling language-specific features.

## Troubleshooting

**Backend not found**: Ensure your backend is added to `backend-registry` in `backends.rkt`.

**Compilation errors**: Check that your emitter handles all IR node types. Use `--dump-ir` to see the normalized IR:
```bash
omnilisp --dump-ir -t yourbackend input.rkt
```

**Test failures**: Run tests with verbose output:
```bash
racket tests/transpiler.rkt -v
```

## Architecture Benefits

This pluggable design provides:

- **Separation of concerns**: Core transpiler logic is independent of target languages
- **Easy extensibility**: New backends require no changes to existing code
- **Consistent interface**: All backends use the same IR, ensuring consistent semantics
- **Discoverability**: `--list-targets` automatically includes new backends
- **Testability**: Each backend can be tested independently

## Future Enhancements

Potential improvements to the backend system:

- Backend-specific optimization passes
- Target-specific IR extensions
- Backend capability flags (e.g., supports-closures, supports-gc)
- Plugin system for loading backends from external packages
- Backend configuration options (e.g., optimization level, runtime library)
