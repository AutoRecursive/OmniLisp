#lang racket

;; Target backend: cpp
;; Demonstrates C++ includes and calls, helper functions, conditionals,
;; arithmetic, and list output through the C++ backend runtime helpers.

(cpp-include <cmath>)

(define (square x)
  (* x x))

(define (abs-delta a b)
  (if (> a b)
      (- a b)
      (- b a)))

(define (distance x1 y1 x2 y2)
  (cpp-call (:: std sqrt)
            (+ (square (abs-delta x2 x1))
               (square (abs-delta y2 y1)))))

(define point-a (list 0 0))
(define point-b (list 3 4))
(define length (distance 0 0 3 4))

(displayln point-a)
(displayln point-b)
(displayln length)
