#lang racket

;; OmniLisp Core IR
;;
;; Design Principles:
;;
;; 1. HOST-LANGUAGE NEUTRALITY
;;    The IR is independent of Racket runtime objects, syntax objects,
;;    and expander internals. This allows the IR to be:
;;    - Serialized and deserialized without preserving Racket state
;;    - Processed by tools written in other languages
;;    - Used as a stable interchange format between compilation stages
;;
;; 2. MINIMAL CORE CONSTRUCTS
;;    The IR provides only essential language constructs:
;;    - Literals (numbers, strings, booleans, etc.)
;;    - Variable references
;;    - Function calls (with positional and keyword arguments)
;;    - Conditionals (if-then-else)
;;    - Local bindings (let)
;;    - Lambda expressions
;;    - Attribute access (for object-oriented targets)
;;    - Scoped references (for namespaces in C++/Rust)
;;    - Imports and includes (target-specific)
;;
;; 3. TARGET-AGNOSTIC REPRESENTATION
;;    While the IR supports target-specific constructs (cpp-include,
;;    rust-use, py-import), the core expression forms are universal.
;;    Backend transpilers interpret these forms according to their
;;    target language semantics.
;;
;; 4. NO IMPLICIT SEMANTICS
;;    The IR does not assume:
;;    - Lexical scoping rules (backends decide)
;;    - Type systems (backends may add type inference)
;;    - Evaluation order (backends control sequencing)
;;    - Memory management (backends choose GC, RAII, or manual)

(provide
 (struct-out program)
 (struct-out import-spec)
 (struct-out from-import-spec)
 (struct-out import-name)
 (struct-out include-spec)
 (struct-out define-value)
 (struct-out define-function)
 (struct-out expr-stmt)
 (struct-out literal)
 (struct-out var-ref)
 (struct-out application)
 (struct-out keyword-arg)
 (struct-out if-expr)
 (struct-out let-expr)
 (struct-out binding)
 (struct-out attr-expr)
 (struct-out scope-ref)
 (struct-out construct-expr)
 (struct-out lambda-expr)
 (struct-out list-expr)
 (struct-out dict-entry)
 (struct-out dict-expr)
 (struct-out begin-expr))

;; Top-level program structure
;; forms: (listof (or/c import-spec from-import-spec include-spec
;;                      define-value define-function expr-stmt))
(struct program (forms) #:transparent)

;; Import declarations (target-specific)
;; module: string, alias: (or/c string #f)
(struct import-spec (module alias) #:transparent)
;; module: string, names: (listof import-name)
(struct from-import-spec (module names) #:transparent)
;; name: string, alias: (or/c string #f)
(struct import-name (name alias) #:transparent)
;; target: symbol (cpp, rust), header: string, style: symbol (system, local, use)
(struct include-spec (target header style) #:transparent)

;; Top-level definitions
;; name: string, expr: expr
(struct define-value (name expr) #:transparent)
;; name: string, params: (listof string), body: (listof expr)
(struct define-function (name params body) #:transparent)
;; expr: expr (top-level expression statement)
(struct expr-stmt (expr) #:transparent)

;; Core expression forms
;; value: any Racket value (number, string, boolean, void, null, etc.)
;; Note: literals are host-language values only during IR construction;
;; backends serialize them appropriately for their target language
(struct literal (value) #:transparent)
;; name: string (variable identifier)
(struct var-ref (name) #:transparent)
;; target: expr, positional: (listof expr), keyword: (listof keyword-arg)
(struct application (target positional keyword) #:transparent)
;; name: string (keyword name without colon), expr: expr
(struct keyword-arg (name expr) #:transparent)
;; test: expr, then: expr, else: expr
(struct if-expr (test then else) #:transparent)
;; bindings: (listof binding), body: (listof expr)
(struct let-expr (bindings body) #:transparent)
;; name: string, expr: expr
(struct binding (name expr) #:transparent)
;; target: expr, name: string (attribute/field name)
(struct attr-expr (target name) #:transparent)
;; parts: (listof string) (namespace path like ["std", "vector"])
(struct scope-ref (parts) #:transparent)
;; type: scope-ref, positional: (listof expr), keyword: (listof keyword-arg)
(struct construct-expr (type positional keyword) #:transparent)
;; params: (listof string), body: (listof expr)
(struct lambda-expr (params body) #:transparent)
;; elements: (listof expr)
(struct list-expr (elements) #:transparent)
;; key: expr, value: expr
(struct dict-entry (key value) #:transparent)
;; entries: (listof dict-entry)
(struct dict-expr (entries) #:transparent)
;; exprs: (listof expr) (sequential evaluation)
(struct begin-expr (exprs) #:transparent)
