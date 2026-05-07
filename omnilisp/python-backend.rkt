#lang racket

(require racket/list
         racket/match
         racket/set
         racket/string
         "ir.rkt")

(provide emit-program)

(struct lowered (statements value) #:transparent)

(define current-name-counter (make-parameter 0))
(define current-runtime-imports (make-parameter (mutable-set)))

(define python-keywords
  (set "False" "None" "True" "and" "as" "assert" "async" "await" "break"
       "class" "continue" "def" "del" "elif" "else" "except" "finally"
       "for" "from" "global" "if" "import" "in" "is" "lambda" "nonlocal"
       "not" "or" "pass" "raise" "return" "try" "while" "with" "yield"
       "match" "case"))

(define (emit-program prog)
  (parameterize ([current-name-counter 0]
                 [current-runtime-imports (mutable-set)])
    (define user-blocks
      (map emit-top-level-form (program-forms prog)))
    (define runtime-lines
      (sort (set->list (current-runtime-imports)) string<?))
    (define blocks
      (append (if (null? runtime-lines)
                  '()
                  (list runtime-lines))
              user-blocks))
    (string-join (join-blocks blocks) "\n")))

(define (emit-top-level-form form)
  (match form
    [(import-spec module alias)
     (list (if alias
               (format "import ~a as ~a" module (python-name alias))
               (format "import ~a" module)))]
    [(from-import-spec module names)
     (list
      (format "from ~a import ~a"
              module
              (string-join
               (for/list ([name (in-list names)])
                 (match name
                   [(import-name imported alias)
                    (if alias
                        (format "~a as ~a" imported (python-name alias))
                        imported)]))
               ", ")))]
    [(define-value name expr)
     (emit-assignment name expr)]
    [(define-function name params body)
     (append
      (list (format "def ~a(~a):"
                    (python-name name)
                    (string-join (map python-name params) ", ")))
      (indent-lines (emit-body body)))]
    [(expr-stmt expr)
     (emit-expression-statement expr)]))

(define (emit-assignment name expr)
  (define lowered-expr (lower-expr expr))
  (append (lowered-statements lowered-expr)
          (list (format "~a = ~a"
                        (python-name name)
                        (lowered-value lowered-expr)))))

(define (emit-body exprs)
  (cond
    [(null? exprs)
     (list "return None")]
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
             (list (lowered-value lowered-expr)))]))

(define (emit-return-statement expr)
  (define lowered-expr (lower-expr expr))
  (append (lowered-statements lowered-expr)
          (list (format "return ~a" (lowered-value lowered-expr)))))

(define (lower-expr expr)
  (match expr
    [(literal value)
     (lowered '() (emit-literal value))]
    [(var-ref name)
     (lowered '() (python-name name))]
    [(attr-expr target name)
     (define lowered-target (lower-expr target))
     (lowered (lowered-statements lowered-target)
              (format "~a.~a"
                      (lowered-value lowered-target)
                      (python-attr-name name)))]
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
                            (list (format "~a: ~a"
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
                 (format "~a=~a" (car pair) (cdr pair)))))
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
         (string=? name "//")
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
     (format "print(~a)"
             (string-join
              (append positional-values
                      (for/list ([pair (in-list keyword-values)])
                        (format "~a=~a" (car pair) (cdr pair))))
              ", "))]
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
    [(member name '("%" "//" "**"))
     (cond
       [(< arity 2)
        (error 'render-operator-call "~a expects at least two arguments" name)]
       [else (format "(~a)" (string-join positional-values (format " ~a " name)))])]
    [(member name '("=" "equal?" "eq?"))
     (render-comparison-chain "==" positional-values)]
    [(member name '("<" "<=" ">" ">="))
     (render-comparison-chain name positional-values)]
    [(string=? name "and")
     (cond
       [(zero? arity) "True"]
       [else (format "(~a)" (string-join positional-values " and "))])]
    [(string=? name "or")
     (cond
       [(zero? arity) "False"]
       [else (format "(~a)" (string-join positional-values " or "))])]
    [(string=? name "not")
     (unless (= arity 1)
       (error 'render-operator-call "not expects exactly one argument"))
     (format "(not ~a)" (car positional-values))]))

(define (render-comparison-chain operator positional-values)
  (define arity (length positional-values))
  (cond
    [(<= arity 1) "True"]
    [else
     (format "(~a)"
             (string-join positional-values (format " ~a " operator)))]))

(define (emit-discarded-if-expression test then else)
  (define lowered-test (lower-expr test))
  (define then-lines (emit-expression-statement then))
  (define else-lines (emit-expression-statement else))
  (append
   (lowered-statements lowered-test)
   (list (format "if ~a:" (lowered-value lowered-test)))
   (indent-lines then-lines)
   (if (void-literal? else)
       '()
       (append (list "else:")
               (indent-lines else-lines)))))

(define (emit-if-expression test then else)
  (define lowered-test (lower-expr test))
  (define lowered-then (lower-expr then))
  (define lowered-else (lower-expr else))
  (if (and (null? (lowered-statements lowered-then))
           (null? (lowered-statements lowered-else)))
      (lowered (lowered-statements lowered-test)
               (format "(~a if ~a else ~a)"
                       (lowered-value lowered-then)
                       (lowered-value lowered-test)
                       (lowered-value lowered-else)))
      (let ([temp-name (fresh-name "__omni_if_value")])
        (lowered
         (append
          (lowered-statements lowered-test)
          (list (format "~a = None" temp-name)
                (format "if ~a:" (lowered-value lowered-test)))
          (indent-lines
           (append (lowered-statements lowered-then)
                   (list (format "~a = ~a"
                                 temp-name
                                 (lowered-value lowered-then)))))
          (list "else:")
          (indent-lines
           (append (lowered-statements lowered-else)
                   (list (format "~a = ~a"
                                 temp-name
                                 (lowered-value lowered-else))))))
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
    (list (format "def ~a():" helper-name))
    (indent-lines (append binding-lines
                          (emit-body body))))
   (format "~a()" helper-name)))

(define (emit-lambda-expression params body)
  (define helper-name (fresh-name "__omni_lambda"))
  (lowered
   (append
    (list (format "def ~a(~a):"
                  helper-name
                  (string-join (map python-name params) ", ")))
    (indent-lines (emit-body body)))
   helper-name))

(define (emit-begin-expression exprs)
  (define helper-name (fresh-name "__omni_begin"))
  (lowered
   (append
    (list (format "def ~a():" helper-name))
    (indent-lines (emit-body exprs)))
   (format "~a()" helper-name)))

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
                       (list (cons (python-attr-name name)
                                   (lowered-value lowered-expr)))))])))

(define (emit-literal value)
  (cond
    [(void? value) "None"]
    [(boolean? value) (if value "True" "False")]
    [(integer? value) (number->string value)]
    [(and (real? value) (inexact? value)) (format "~a" value)]
    [(and (real? value) (exact? value))
     (cond
       [(integer? value) (number->string value)]
       [else
        (register-runtime-import! "from fractions import Fraction")
        (format "Fraction(~a, ~a)"
                (numerator value)
                (denominator value))])]
    [(complex? value)
     (format "complex(~a, ~a)"
             (emit-literal (real-part value))
             (emit-literal (imag-part value)))]
    [(string? value) (~s value)]
    [(char? value) (~s (string value))]
    [(bytes? value)
     (format "bytes([~a])"
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
      (list (string-append prefix "pass"))
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

(define (register-runtime-import! line)
  (set-add! (current-runtime-imports) line))

(define (python-name raw-name)
  (define sanitized
    (sanitize-identifier raw-name))
  (if (set-member? python-keywords sanitized)
      (string-append sanitized "_")
      sanitized))

(define (python-attr-name raw-name)
  (sanitize-identifier raw-name))

(define (sanitize-identifier raw-name)
  (define input
    (cond
      [(symbol? raw-name) (symbol->string raw-name)]
      [(string? raw-name) raw-name]
      [else (error 'sanitize-identifier "identifier must be a string or symbol: ~e" raw-name)]))
  (define normalized
    (regexp-replace* #px"[^A-Za-z0-9_]" (string-replace input "-" "_") "_"))
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
