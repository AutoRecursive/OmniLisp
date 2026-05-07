# OmniLisp Agent Notes

## Current Direction

OmniLisp is implemented in Racket, but its core IR must not depend on Racket runtime objects,
syntax objects, or expander internals. Racket is the implementation language for the compiler,
not the semantic shape of the IR.

The compiler pipeline should follow this shape:

1. Source reader / frontend
2. Selective macro expansion or surface-form desugaring into OmniLisp Core
3. Neutral IR construction
4. Target backend lowering / code generation

## Architectural Rules

### 1. Racket is the host, not the IR

Do not let the IR depend on:
- `syntax?` objects
- expander scopes or module-path internals
- host-language namespaces or runtime values beyond plain literals

The IR may use only plain data and explicit structs such as:
- literals
- variable references
- calls with positional and keyword arguments
- branching
- bindings
- attribute access
- imports

### 2. Expand to a controlled core language

We do not want to fully lower source programs into raw Racket kernel forms and then emit target
code from those kernel forms. That would overfit the pipeline to Racket internals and lose useful
high-level structure.

Instead, the frontend should normalize surface constructs into a controlled OmniLisp Core. In the
short term this can be implemented with explicit desugaring for derived forms such as `when`,
`unless`, and `cond`. In the longer term, we can integrate with Racket's expander more deeply,
but the output of that phase should still be projected into our own core forms before reaching IR.

### 3. The IR is target-neutral

Although Python is the first backend, the IR should model semantics that can be consumed by any
backend.

Examples:
- imports are generic module imports, not Python-only nodes
- function calls support positional and keyword arguments generically
- chained access is represented as nested attribute nodes
- list and hash literals are generic collections

Python-specific surface forms such as `py-import` or `py-call` are frontend conveniences. They
must lower into generic IR nodes instead of leaking Python-specific semantics into the IR itself.

### 4. Backends are pluggable

The transpiler entrypoint must not be hardcoded to Python. It should dispatch through a backend
registry keyed by target name, with Python registered as the first backend.

This keeps the overall shape:

- one frontend / IR pipeline
- many backends (`python`, future `javascript`, `cpp`, `haskell`, ...)

### 5. Preserve room for lexical identity

As macro support becomes more advanced, the frontend should preserve binding identity long enough
to avoid hygiene bugs. A later pass can rename bindings into backend-safe identifiers.

The current implementation may still use plain names for a first cut, but the architecture should
assume a future unique-binding layer.

## Immediate Implementation Plan

1. Keep the current neutral IR structs and treat them as OmniLisp Core IR.
2. Refactor transpilation so targets are resolved through a backend registry.
3. Keep Python as the first backend, but remove Python-only assumptions from the top-level API.
4. Add frontend desugaring for a few core derived forms to make the source-to-core boundary
   explicit.
5. Add tests for target selection and core desugaring.

## Non-Goals For This Iteration

- full Racket macro expander integration
- hygiene-preserving identifier graphs
- non-Python production backends
- optimization passes

Those can come after the neutral pipeline boundary is stable.
