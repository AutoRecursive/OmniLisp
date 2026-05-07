#lang racket

;; Target backend: rust
;; Demonstrates Rust use declarations, functions, conditionals, list literals,
;; and arithmetic with an imported standard-library constant.

(rust-use std::f64::consts::PI)

(define (circle-area radius)
  (* PI radius radius))

(define (size-label radius)
  (if (> radius 10.0)
      "large circle"
      "small circle"))

(define radii (list 2.0 5.0 12.0))
(define selected-radius 5.0)
(define selected-area (circle-area selected-radius))

(displayln radii)
(displayln (size-label selected-radius))
(displayln selected-area)
