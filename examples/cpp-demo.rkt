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
