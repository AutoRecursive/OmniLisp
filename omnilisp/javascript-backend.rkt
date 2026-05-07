#lang racket

(require racket/list
         racket/match
         racket/set
         racket/string
         "ir.rkt")

(provide emit-program)

(struct lowered (statements value) #:transparent)

(define current-name-counter (make-parameter 0))

(define javascript-keywords
  (set "await" "break" "case" "catch" "class" "const" "continue" "debugger"
       "default" "delete" "do" "else" "enum" "export" "extends" "false"
       "finally" "for" "function" "if" "import" "in" "instanceof" "let"
       "new" "null" "return" "super" "switch" "this" "throw" "true"
       "try" "typeof" "var" "void" "while" "with" "yield"))

(define (emit-program prog)
  (parameterize ([current-name-counter 0])
    (define user-blocks
      (map emit-top-level-form (program-forms prog)))
    (define blocks user-blocks)
    (string-join (join-blocks blocks) "\n")))

(define (emit-top-level-form form)
  (match form
    [(import-spec module alias)
     (list (if alias
               (format "import * as ~a from '~a';" (js-name alias) module)
               (format "import '~a';" module)))]
    [(from-import-spec module names)
     (list
      (format "import { ~a } from '~a';"
              (string-join
               (for/list ([name (in-list names)])
                 (match name
                   [(import-name imported alias)
                    (if alias
                        (format "~a as ~a" imported (js-name alias))
                        imported)]))
               ", ")
              module))]
    [(define-value name expr)
     (emit-assignment name expr)]
    [(define-function name params body)
     (append
      (list (format "function ~a(~a) {"
                    (js-name name)
                    (string-join (map js-name params) ", ")))
      (indent-lines (emit-body body))
      (list "}"))]
    [(expr-stmt expr)
     (emit-expression-statement expr)]))

(define (emit-assignment name expr)
  (define lowered-expr (lower-expr expr))
  (append (lowered-statements lowered-expr)
          (list (format "const ~a = ~a;"
                        (js-name name)
                        (lowered-value lowered-expr)))))

(define (emit-body exprs)
  (cond
    [(null? exprs)
     (list "return undefined;")]
    [else
     (append
      (append-map emit-expression-statement (drop-right exprs 1))
      (emit-return-statement (last exprs)))]))

(define (emit-expression-statement expr)
  (match expr
    [(if-expr test then else)
     (emit-discarded-if-expression test then else)]
    [(begin-expr exprs)
     (append-map emit-expression-statement exprs)]
    [_
     (define lowered-expr (lower-expr expr))
     (append (lowered-statements lowered-expr)
             (list (format "~a;" (lowered-value lowered-expr))))]))

(define (emit-return-statement expr)
  (define lowered-expr (lower-expr expr))
  (append (lowered-statements lowered-expr)
          (list (format "return ~a;" (lowered-value lowered-expr)))))

(define (lower-expr expr)
  (match expr
    [(literal value)
     (lowered '() (emit-literal value))]
    [(var-ref name)
     (lowered '() (js-name name))]
    [(attr-expr target name)
     (define lowered-target (lower-expr target))
     (lowered (lowered-statements lowered-target)
              (format "~a.~a"
                      (lowered-value lowered-target)
                      (js-attr-name name)))]
    [(list-expr elements)
     (define-values (statements values) (lower-many elements))
     (lowered statements
              (format "[~a]" (string-join values ", ")))]
    [(dict-expr entries)
     (define-values (statements pieces)
       (for/fold ([all-statements '()]
                  [all-pieces '()])
                 ([entry (in-list entries)])
         (match entry
           [(dict-entry key value)
            (define lowered-key (lower-expr key))
            (define lowered-value* (lower-expr value))
            (values (append all-statements
                            (lowered-statements lowered-key)
                            (lowered-statements lowered-value*))
                    (append all-pieces
                            (list (format "[~a]: ~a"
                                          (lowered-value lowered-key)
                                          (lowered-value lowered-value*)))))])))
     (lowered statements
              (format "{~a}" (string-join pieces ", ")))]
    [(application target positional keyword)
     (emit-application target positional keyword)]
    [(if-expr test then else)
     (emit-if-expression test then else)]
    [(let-expr bindings body)
     (emit-let-expression bindings body)]
    [(lambda-expr params body)
     (emit-lambda-expression params body)]
    [(begin-expr exprs)
     (emit-begin-expression exprs)]))

(define (emit-application target positional keyword)
  (define lowered-target (lower-expr target))
  (define-values (positional-statements positional-values)
    (lower-many positional))
  (define-values (keyword-statements keyword-values)
    (lower-keywords keyword))
  (define setup
    (append (lowered-statements lowered-target)
            positional-statements
            keyword-statements))
  (lowered setup
           (emit-call-expression target
                                 (lowered-value lowered-target)
                                 positional-values
                                 keyword-values)))

(define (emit-call-expression target target-value positional-values keyword-values)
  (cond
    [(and (var-ref? target)
          (render-special-call (var-ref-name target) positional-values keyword-values))
     => values]
    [else
     (define argument-values
       (append positional-values
               (for/list ([pair (in-list keyword-values)])
                 (format "~a: ~a" (car pair) (cdr pair)))))
     (format "~a(~a)"
             target-value
             (string-join argument-values ", "))]))

(define (render-special-call name positional-values keyword-values)
  (cond
    [(or (string=? name "+")
         (string=? name "-")
         (string=? name "*")
         (string=? name "/")
         (string=? name "%")
         (string=? name "**")
         (string=? name "=")
         (string=? name "equal?")
         (string=? name "eq?")
         (string=? name "<")
         (string=? name "<=")
         (string=? name ">")
         (string=? name ">=")
         (string=? name "and")
         (string=? name "or")
         (string=? name "not"))
     (when (pair? keyword-values)
       (error 'render-special-call "operator does not accept keyword arguments: ~a" name))
     (render-operator-call name positional-values)]
    [(or (string=? name "display")
         (string=? name "displayln")
         (string=? name "print"))
     (format "console.log(~a)"
             (string-join positional-values ", "))]
    [else #f]))

(define (render-operator-call name positional-values)
  (define arity (length positional-values))
  (define (binary-join operator default-single)
    (cond
      [(zero? arity) default-single]
      [(= arity 1) (car positional-values)]
      [else (format "(~a)" (string-join positional-values (format " ~a " operator)))]))
  (cond
    [(string=? name "+") (binary-join "+" "0")]
    [(string=? name "*") (binary-join "*" "1")]
    [(string=? name "-")
     (cond
       [(zero? arity) (error 'render-operator-call "- expects at least one argument")]
       [(= arity 1) (format "(-~a)" (car positional-values))]
       [else (format "(~a)" (string-join positional-values " - "))])]
    [(string=? name "/")
     (cond
       [(zero? arity) (error 'render-operator-call "/ expects at least one argument")]
       [(= arity 1) (format "(1 / ~a)" (car positional-values))]
       [else (format "(~a)" (string-join positional-values " / "))])]
    [(string=? name "%")
     (cond
       [(< arity 2)
        (error 'render-operator-call "% expects at least two arguments")]
       [else (format "(~a)" (string-join positional-values " % "))])]
    [(string=? name "**")
     (cond
       [(< arity 2)
        (error 'render-operator-call "** expects at least two arguments")]
       [else (format "(~a)" (string-join positional-values " ** "))])]
    [(member name '("=" "equal?" "eq?"))
     (render-comparison-chain "===" positional-values)]
    [(member name '("<" "<=" ">" ">="))
     (render-comparison-chain name positional-values)]
    [(string=? name "and")
     (cond
       [(zero? arity) "true"]
       [else (format "(~a)" (string-join positional-values " && "))])]
    [(string=? name "or")
     (cond
       [(zero? arity) "false"]
       [else (format "(~a)" (string-join positional-values " || "))])]
    [(string=? name "not")
     (unless (= arity 1)
       (error 'render-operator-call "not expects exactly one argument"))
     (format "(!~a)" (car positional-values))]))

(define (render-comparison-chain operator positional-values)
  (define arity (length positional-values))
  (cond
    [(<= arity 1) "true"]
    [else
     (format "(~a)"
             (string-join positional-values (format " ~a " operator)))]))

(define (emit-discarded-if-expression test then else)
  (define lowered-test (lower-expr test))
  (define then-lines (emit-expression-statement then))
  (define else-lines (emit-expression-statement else))
  (append
   (lowered-statements lowered-test)
   (list (format "if (~a) {" (lowered-value lowered-test)))
   (indent-lines then-lines)
   (if (void-literal? else)
       (list "}")
       (append (list "} else {")
               (indent-lines else-lines)
               (list "}")))))

(define (emit-if-expression test then else)
  (define lowered-test (lower-expr test))
  (define lowered-then (lower-expr then))
  (define lowered-else (lower-expr else))
  (if (and (null? (lowered-statements lowered-then))
           (null? (lowered-statements lowered-else)))
      (lowered (lowered-statements lowered-test)
               (format "(~a ? ~a : ~a)"
                       (lowered-value lowered-test)
                       (lowered-value lowered-then)
                       (lowered-value lowered-else)))
      (let ([temp-name (fresh-name "__omni_if_value")])
        (lowered
         (append
          (lowered-statements lowered-test)
          (list (format "let ~a;" temp-name)
                (format "if (~a) {" (lowered-value lowered-test)))
          (indent-lines
           (append (lowered-statements lowered-then)
                   (list (format "~a = ~a;"
                                 temp-name
                                 (lowered-value lowered-then)))))
          (list "} else {")
          (indent-lines
           (append (lowered-statements lowered-else)
                   (list (format "~a = ~a;"
                                 temp-name
                                 (lowered-value lowered-else)))))
          (list "}"))
         temp-name))))

(define (emit-let-expression bindings body)
  (define helper-name (fresh-name "__omni_let"))
  (define binding-lines
    (append-map
     (lambda (one-binding)
       (match one-binding
         [(binding name value)
          (emit-assignment name value)]))
     bindings))
  (lowered
   (append
    (list (format "const ~a = (() => {" helper-name))
    (indent-lines (append binding-lines
                          (emit-body body)))
    (list "})();"))
   helper-name))

(define (emit-lambda-expression params body)
  (lowered
   '()
   (format "(~a) => { ~a }"
           (string-join (map js-name params) ", ")
           (string-join (emit-body body) " "))))

(define (emit-begin-expression exprs)
  (define helper-name (fresh-name "__omni_begin"))
  (lowered
   (append
    (list (format "const ~a = (() => {" helper-name))
    (indent-lines (emit-body exprs))
    (list "})();"))
   helper-name))

(define (lower-many exprs)
  (for/fold ([all-statements '()]
             [all-values '()])
            ([expr (in-list exprs)])
    (define lowered-expr (lower-expr expr))
    (values (append all-statements
                    (lowered-statements lowered-expr))
            (append all-values
                    (list (lowered-value lowered-expr))))))

(define (lower-keywords keyword-args)
  (for/fold ([all-statements '()]
             [all-values '()])
            ([arg (in-list keyword-args)])
    (match arg
      [(keyword-arg name expr)
       (define lowered-expr (lower-expr expr))
       (values (append all-statements
                       (lowered-statements lowered-expr))
               (append all-values
                       (list (cons (js-attr-name name)
                                   (lowered-value lowered-expr)))))])))

(define (emit-literal value)
  (cond
    [(void? value) "undefined"]
    [(boolean? value) (if value "true" "false")]
    [(integer? value) (number->string value)]
    [(and (real? value) (inexact? value)) (format "~a" value)]
    [(and (real? value) (exact? value))
     (cond
       [(integer? value) (number->string value)]
       [else (format "~a" (exact->inexact value))])]
    [(complex? value)
     (error 'emit-literal "JavaScript does not support complex numbers: ~e" value)]
    [(string? value) (~s value)]
    [(char? value) (~s (string value))]
    [(bytes? value)
     (format "new Uint8Array([~a])"
             (string-join
              (for/list ([byte (in-bytes value)])
                (number->string byte))
              ", "))]
    [(symbol? value) (~s (symbol->string value))]
    [(keyword? value) (~s (keyword->string value))]
    [(vector? value)
     (format "[~a]"
             (string-join
              (for/list ([item (in-vector value)])
                (emit-literal item))
              ", "))]
    [(hash? value)
     (format "{~a}"
             (string-join
              (for/list ([entry (in-list (sort (hash->list value)
                                               string<?
                                               #:key (lambda (item)
                                                       (format "~v" (car item)))))])
                (format "~a: ~a"
                        (emit-literal (car entry))
                        (emit-literal (cdr entry))))
              ", "))]
    [(list? value)
     (format "[~a]"
             (string-join (map emit-literal value) ", "))]
    [else
     (error 'emit-literal "unsupported literal value: ~e" value)]))

(define (indent-lines lines [level 1])
  (define prefix (make-string (* 4 level) #\space))
  (if (null? lines)
      (list (string-append prefix "// empty"))
      (for/list ([line (in-list lines)])
        (if (string=? line "")
            ""
            (string-append prefix line)))))

(define (join-blocks blocks)
  (cond
    [(null? blocks) '()]
    [else
     (let loop ([remaining (cdr blocks)]
                [acc (car blocks)])
       (cond
         [(null? remaining) acc]
         [else
          (loop (cdr remaining)
                (append acc (list "") (car remaining)))]))]))

(define (fresh-name stem)
  (define next-id (add1 (current-name-counter)))
  (current-name-counter next-id)
  (format "~a_~a" stem next-id))

(define (js-name raw-name)
  (define sanitized
    (sanitize-identifier raw-name))
  (if (set-member? javascript-keywords sanitized)
      (string-append sanitized "_")
      sanitized))

(define (js-attr-name raw-name)
  (sanitize-identifier raw-name))

(define (sanitize-identifier raw-name)
  (define input
    (cond
      [(symbol? raw-name) (symbol->string raw-name)]
      [(string? raw-name) raw-name]
      [else (error 'sanitize-identifier "identifier must be a string or symbol: ~e" raw-name)]))
  (define normalized
    (regexp-replace* #px"[^A-Za-z0-9_$]" (string-replace input "-" "_") "_"))
  (define collapsed
    (regexp-replace* #px"_+" normalized "_"))
  (define with-prefix
    (cond
      [(string=? collapsed "") "value"]
      [(regexp-match? #px"^[0-9]" collapsed) (string-append "_" collapsed)]
      [else collapsed]))
  with-prefix)

(define (void-literal? expr)
  (and (literal? expr)
       (void? (literal-value expr))))
