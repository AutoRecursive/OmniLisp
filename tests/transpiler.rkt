#lang racket

(require rackunit
         rackunit/text-ui
         racket/file
         racket/match
         "../omnilisp/ir.rkt"
         "../omnilisp/transpiler.rkt")

(define parser-tests
  (test-suite
   "frontend core lowering"
   (test-case "when lowers into a neutral if expression"
     (define ir
       (parse-source-string
        "#lang racket\n(define (f x) (when x (displayln \"ok\")))"
        'when-sample))
     (match ir
       [(program
         (list
          (define-function "f" '("x")
            (list (if-expr (var-ref "x") _ (literal else-value))))))
        (check-true (void? else-value))]
       [_
        (fail "expected the surface 'when' form to lower into define-function + if-expr")]))
   (test-case "cond lowers into nested if expressions"
     (define ir
       (parse-source-string
        "#lang racket\n(define answer (cond [(> x 0) 1] [else 0]))"
        'cond-sample))
     (match ir
       [(program (list (define-value "answer" (if-expr _ (literal 1) (literal 0)))))
        (void)]
       [_
        (fail "expected cond to lower into a neutral if-expr")]))))

(define backend-tests
  (test-suite
   "backend dispatch"
   (test-case "python target stays behind the backend registry"
     (check-equal? (available-targets) '(python cpp rust javascript))
     (define code
       (transpile-string
        "#lang racket\n(import (numpy np) (:from pandas DataFrame))\n(py-call np.array (list 1 2 3) #:dtype \"float64\")"
        'backend-sample
        #:target "python"))
     (check-not-false (regexp-match? #rx"import numpy as np" code))
     (check-not-false (regexp-match? #rx"from pandas import DataFrame" code))
     (check-not-false (regexp-match? #rx"np\\.array\\(\\[1, 2, 3\\], dtype=\"float64\"\\)" code)))
   (test-case "unknown backend fails loudly"
     (check-exn exn:fail?
                (lambda ()
                  (transpile-string "#lang racket\n42" #:target 'javascript))))
   (test-case "cpp target emits C++20 code"
     (define code
       (transpile-string
        "#lang racket\n(cpp-include <cmath>)\n(define root (cpp-call (:: std sqrt) 16.0))\n(define (score x) (if (> x 10) (+ x 2) (- x 2)))\n(define values (list 1 2 3))\n(displayln (score 12))\n(displayln values)"
        'cpp-sample
        #:target 'cpp))
     (check-not-false (regexp-match? #rx"#include <iostream>" code))
     (check-not-false (regexp-match? #rx"#include <cmath>" code))
     (check-not-false (regexp-match? #rx"std::sqrt\\(16\\.0\\)" code))
     (check-not-false (regexp-match? #rx"auto score\\(auto x\\)" code))
     (check-not-false (regexp-match? #rx"std::vector\\{1, 2, 3\\}" code))
     (check-not-false (regexp-match? #rx"int main\\(\\)" code)))
   (test-case "rust target emits Rust code"
     (define code
       (transpile-string
        "#lang racket\n(rust-use std::f64::consts::PI)\n(define (score x) (if (> x 10) (+ x 2) (- x 2)))\n(define values (list 1 2 3))\n(define area (* PI 2.0 2.0))\n(displayln area)\n(displayln (score 12))\n(displayln values)"
        'rust-sample
        #:target 'rust))
     (check-not-false (regexp-match? #rx"use std::f64::consts::PI;" code))
     (check-not-false (regexp-match? #rx"fn main\\(\\)" code))
     (check-not-false (regexp-match? #rx"let score = \\|x\\|" code))
     (check-not-false (regexp-match? #rx"vec!\\[1, 2, 3\\]" code)))
   (test-case "javascript target emits JavaScript ES6+ code"
     (define code
       (transpile-string
        "#lang racket\n(import (:from math sqrt))\n(define (score x) (if (> x 10) (+ x 2) (- x 2)))\n(define values (list 1 2 3))\n(define result (+ 5 10))\n(displayln result)\n(displayln (score 12))\n(displayln values)"
        'javascript-sample
        #:target 'javascript))
     (check-not-false (regexp-match? #rx"import \\{ sqrt \\} from 'math';" code))
     (check-not-false (regexp-match? #rx"function score\\(" code))
     (check-not-false (regexp-match? #rx"\\[1, 2, 3\\]" code))
     (check-not-false (regexp-match? #rx"console\\.log" code)))))

(define compile-tests
  (test-suite
   "native compile smoke tests"
   (test-case "generated JavaScript runs with Node.js"
     (define node (find-executable-path "node"))
     (unless node
       (fail "no Node.js runtime found in PATH"))
     (define temp-dir (make-temporary-file "omnilisp-js-test-~a" 'directory))
     (define source-path (build-path temp-dir "sample.js"))
     (define output-path (build-path temp-dir "output.txt"))
     (define code
       (transpile-string
        "#lang racket\n(define (score x) (if (> x 10) (+ x 2) (- x 2)))\n(define values (list 1 2 3))\n(define result (+ 5 10))\n(displayln result)\n(displayln (score 12))\n(displayln values)"
        'js-compile-sample
        #:target 'javascript))
     (display-to-file code source-path #:exists 'replace)
     (define run-ok?
       (with-output-to-file output-path
         (lambda ()
           (system* node (path->string source-path)))
         #:exists 'replace))
     (check-true run-ok?)
     (check-equal? (file->string output-path) "15\n14\n[ 1, 2, 3 ]\n"))
   (test-case "generated C++ compiles and runs"
     (define cxx (or (find-executable-path "c++")
                     (find-executable-path "clang++")
                     (find-executable-path "g++")))
     (unless cxx
       (fail "no C++ compiler found in PATH"))
     (define temp-dir (make-temporary-file "omnilisp-cpp-test-~a" 'directory))
     (define source-path (build-path temp-dir "sample.cpp"))
     (define binary-path (build-path temp-dir "sample"))
     (define output-path (build-path temp-dir "output.txt"))
     (define code
       (transpile-string
        "#lang racket\n(cpp-include <cmath>)\n(define root (cpp-call (:: std sqrt) 16.0))\n(define label (cpp-construct (:: std string) \"root\"))\n(define (score x) (if (> x 10) (+ x 2) (- x 2)))\n(displayln label)\n(displayln root)\n(displayln (score 12))\n(displayln (list 1 2 3))"
        'cpp-compile-sample
        #:target 'cpp))
     (display-to-file code source-path #:exists 'replace)
     (define compile-ok?
       (system* cxx "-std=c++20" (path->string source-path) "-o" (path->string binary-path)))
     (check-true compile-ok?)
     (define run-ok?
       (with-output-to-file output-path
         (lambda ()
           (system* binary-path))
         #:exists 'replace))
     (check-true run-ok?)
     (check-equal? (file->string output-path) "root\n4\n14\n[1, 2, 3]\n"))
   (test-case "generated Rust compiles and runs"
     (define rustc (find-executable-path "rustc"))
     (unless rustc
       (fail "no Rust compiler found in PATH"))
     (define temp-dir (make-temporary-file "omnilisp-rust-test-~a" 'directory))
     (define source-path (build-path temp-dir "sample.rs"))
     (define binary-path (build-path temp-dir "sample"))
     (define output-path (build-path temp-dir "output.txt"))
     (define code
       (transpile-string
        "#lang racket\n(rust-use std::f64::consts::PI)\n(define (score x) (if (> x 10) (+ x 2) (- x 2)))\n(define values (list 1 2 3))\n(define radius 2.0)\n(define area (* PI radius radius))\n(displayln area)\n(displayln (score 12))\n(displayln values)"
        'rust-compile-sample
        #:target 'rust))
     (display-to-file code source-path #:exists 'replace)
     (define compile-ok?
       (system* rustc (path->string source-path) "-o" (path->string binary-path)))
     (check-true compile-ok?)
     (define run-ok?
       (with-output-to-file output-path
         (lambda ()
           (system* binary-path))
         #:exists 'replace))
     (check-true run-ok?)
     (check-equal? (file->string output-path) "12.566370614359172\n14\n[1, 2, 3]\n"))))

(module+ test
  (run-tests parser-tests)
  (run-tests backend-tests)
  (run-tests compile-tests))
