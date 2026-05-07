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
