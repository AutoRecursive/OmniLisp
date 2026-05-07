#lang racket

(require racket/list
         racket/match
         racket/string
         (only-in "cpp-backend.rkt" [emit-program emit-cpp-program])
         (only-in "python-backend.rkt" [emit-program emit-python-program])
         (only-in "rust-backend.rkt" [emit-program emit-rust-program]))

(provide (struct-out backend)
         available-backends
         available-targets
         get-backend
         emit-program)

(struct backend (id description emitter) #:transparent)

(define backend-registry
  (list
   (backend 'python
            "Python backend with support for keyword arguments, imports, attrs, and collection literals."
            emit-python-program)
   (backend 'cpp
            "C++20 backend for the neutral IR core subset."
            emit-cpp-program)
   (backend 'rust
            "Rust backend for the neutral IR core subset."
            emit-rust-program)))

(define (available-backends)
  backend-registry)

(define (available-targets)
  (map backend-id backend-registry))

(define (get-backend target)
  (define normalized-target (normalize-target target))
  (or (findf (lambda (one-backend)
               (eq? (backend-id one-backend) normalized-target))
             backend-registry)
      (error 'get-backend
             "unknown backend target ~a; available targets: ~a"
             target
             (string-join (map symbol->string (available-targets)) ", "))))

(define (emit-program prog #:target [target 'python])
  ((backend-emitter (get-backend target)) prog))

(define (normalize-target target)
  (match target
    [(? symbol?) target]
    [(? string?) (string->symbol (string-downcase target))]
    [_
     (error 'normalize-target "target must be a symbol or string: ~e" target)]))
