#lang racket

;; Target backend: javascript
;; Demonstrates recursive functions, nested conditionals, boolean operators,
;; and lists that compile to JavaScript arrays.

(define (fib n)
  (if (<= n 1)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (label n)
  (if (and (>= n 0) (<= n 10))
      "small fibonacci request"
      "large fibonacci request"))

(define sample-values (list 0 1 2 3 5 8))
(define requested 7)

(displayln (label requested))
(displayln sample-values)
(displayln (fib requested))
