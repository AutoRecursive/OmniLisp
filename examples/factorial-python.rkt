#lang racket

;; Target backend: python
;; Demonstrates a recursive function, an if conditional, a list literal,
;; and a Python module import/call for backend-specific interop.

(py-import math)

(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(define inputs (list 3 4 5))
(define result (factorial 5))
(define root (py-call math.sqrt result))

(displayln "factorial inputs")
(displayln inputs)
(displayln result)
(displayln root)
