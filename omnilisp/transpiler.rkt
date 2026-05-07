#lang racket

(require "backends.rkt"
         "parser.rkt")

(provide (struct-out backend)
         parse-source-file
         parse-source-string
         available-backends
         available-targets
         transpile-program
         transpile-file
         transpile-string)

(define (parse-source-file path)
  (parse-file path))

(define (parse-source-string source [source-name 'string])
  (parse-string source source-name))

(define (transpile-program prog #:target [target 'python])
  (emit-program prog #:target target))

(define (transpile-file path #:target [target 'python])
  (transpile-program (parse-source-file path)
                     #:target target))

(define (transpile-string source [source-name 'string] #:target [target 'python])
  (transpile-program (parse-source-string source source-name)
                     #:target target))
