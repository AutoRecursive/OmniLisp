#lang racket

(require racket/list
         racket/match
         racket/set
         racket/string
         "ir.rkt")

(provide emit-program)

(struct lowered (statements value) #:transparent)

(define current-name-counter (make-parameter 0))

(define rust-keywords
  (set "as" "async" "await" "break" "const" "continue" "crate" "dyn" "else"
       "enum" "extern" "false" "fn" "for" "if" "impl" "in" "let" "loop"
       "match" "mod" "move" "mut" "pub" "ref" "return" "self" "Self"
       "static" "struct" "super" "trait" "true" "type" "unsafe" "use"
       "where" "while"))

(define (emit-program prog)
  (parameterize ([current-name-counter 0])
    (define-values (main-lines use-lines comments)
      (partition-top-level (program-forms prog)))
    (define blocks
      (append
       (list (runtime-attribute-lines))
       (if (null? use-lines) '() (list use-lines))
       (if (null? comments) '() (list comments))
       (list (append (list "fn main() {")
                     (indent-lines main-lines)
                     (list "}")))))
    (string-join (join-blocks blocks) "\n")))

(define (runtime-attribute-lines)
  (list "#![allow(unused_imports)]"
        "#![allow(unused_parens)]"))

(define (partition-top-level forms)
  (for/fold ([main-lines '()]
             [use-lines '()]
             [comments '()])
            ([form (in-list forms)])
    (match form
      [(include-spec target path style)
       (cond
         [(and (eq? target 'rust) (eq? style 'use))
          (values main-lines
                  (append use-lines (list (format "use ~a;" path)))
                  comments)]
         [else
          (values main-lines
                  use-lines
                  (append comments
                          (list (format "// ignored include for target ~e: ~a"
                                        target
                                        path))))])]
      [(import-spec module alias)
       (values main-lines
               use-lines
               (append comments
                       (list (format "// import ~a~a"
                                     module
                                     (if alias
                                         (format " as ~a" (rust-name alias))
                                         "")))))]
      [(from-import-spec module names)
       (values main-lines
               use-lines
               (append comments
                       (list (format "// from ~a import ~a"
                                     module
                                     (string-join
                                      (for/list ([name (in-list names)])
                                        (match name
                                          [(import-name imported alias)
                                           (if alias
                                               (format "~a as ~a"
                                                       imported
                                                       (rust-name alias))
                                               imported)]))
                                      ", ")))))]
      [(define-function name params body)
       (values (append main-lines
                       (emit-closure-binding name params body))
               use-lines
               comments)]
      [(define-value name expr)
       (values (append main-lines (emit-assignment name expr))
               use-lines
               comments)]
      [(expr-stmt expr)
       (values (append main-lines (emit-expression-statement expr))
               use-lines
               comments)])))

(define (emit-closure-binding name params body)
  (append
   (list (format "let ~a = |~a| {"
                 (rust-name name)
                 (string-join (map rust-name params) ", ")))
   (indent-lines (emit-body body))
   (list "};")))

(define (emit-assignment name expr)
  (define lowered-expr (lower-expr expr))
  (append (lowered-statements lowered-expr)
          (list (format "let ~a = ~a;"
                        (rust-name name)
                        (lowered-value lowered-expr)))))

(define (emit-body exprs)
  (cond
    [(null? exprs) (list "()")]
    [else
     (append
      (append-map emit-expression-statement (drop-right exprs 1))
      (emit-tail-expression (last exprs)))]))

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

(define (emit-tail-expression expr)
  (define lowered-expr (lower-expr expr))
  (append (lowered-statements lowered-expr)
          (list (lowered-value lowered-expr))))

(define (lower-expr expr)
  (match expr
    [(literal value)
     (lowered '() (emit-literal value))]
    [(var-ref name)
     (lowered '() (rust-name name))]
    [(attr-expr target name)
     (define lowered-target (lower-expr target))
     (lowered (lowered-statements lowered-target)
              (format "~a.~a"
                      (lowered-value lowered-target)
                      (rust-member-name name)))]
    [(scope-ref parts)
     (lowered '() (render-scope-ref parts))]
    [(construct-expr type positional keyword)
     (emit-construct-expression type positional keyword)]
    [(list-expr elements)
     (define-values (statements values) (lower-many elements))
     (lowered statements
              (format "vec![~a]" (string-join values ", ")))]
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
                            (list (format "(~a, ~a)"
                                          (lowered-value lowered-key)
                                          (lowered-value lowered-value*)))))])))
     (lowered statements
              (format "std::collections::BTreeMap::from([~a])"
                      (string-join pieces ", ")))]
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
  (when (pair? keyword)
    (error 'emit-rust-application
           "Rust backend does not support keyword arguments: ~e"
           keyword))
  (define lowered-target (lower-expr target))
  (define-values (positional-statements positional-values)
    (lower-many positional))
  (lowered (append (lowered-statements lowered-target)
                   positional-statements)
           (emit-call-expression target
                                 (lowered-value lowered-target)
                                 positional-values)))

(define (emit-call-expression target target-value positional-values)
  (cond
    [(and (var-ref? target)
          (render-special-call (var-ref-name target) positional-values))
     => values]
    [else
     (format "~a(~a)"
             target-value
             (string-join positional-values ", "))]))

(define (render-special-call name positional-values)
  (cond
    [(operator-name? name)
     (render-operator-call name positional-values)]
    [(or (string=? name "display")
         (string=? name "displayln")
         (string=? name "print"))
     (if (null? positional-values)
         "println!()"
         (format "println!(\"{:?}\", ~a)"
                 (if (= (length positional-values) 1)
                     (car positional-values)
                     (format "(~a)" (string-join positional-values ", ")))))]
    [else #f]))

(define (operator-name? name)
  (member name '("+" "-" "*" "/" "%" "=" "equal?" "eq?"
                 "<" "<=" ">" ">=" "and" "or" "not")))

(define (render-operator-call name positional-values)
  (define arity (length positional-values))
  (define (binary-join operator default-value)
    (cond
      [(zero? arity) default-value]
      [(= arity 1) (car positional-values)]
      [else (format "(~a)" (string-join positional-values (format " ~a " operator)))]))
  (cond
    [(string=? name "+") (binary-join "+" "0")]
    [(string=? name "*") (binary-join "*" "1")]
    [(string=? name "-")
     (cond
       [(zero? arity) (error 'render-rust-operator-call "- expects at least one argument")]
       [(= arity 1) (format "(-~a)" (car positional-values))]
       [else (format "(~a)" (string-join positional-values " - "))])]
    [(string=? name "/")
     (cond
       [(zero? arity) (error 'render-rust-operator-call "/ expects at least one argument")]
       [(= arity 1) (format "(1 / ~a)" (car positional-values))]
       [else (format "(~a)" (string-join positional-values " / "))])]
    [(string=? name "%")
     (cond
       [(< arity 2)
        (error 'render-rust-operator-call "% expects at least two arguments")]
       [else (format "(~a)" (string-join positional-values " % "))])]
    [(member name '("=" "equal?" "eq?"))
     (render-comparison-chain "==" positional-values)]
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
       (error 'render-rust-operator-call "not expects exactly one argument"))
     (format "(!~a)" (car positional-values))]))

(define (render-comparison-chain operator positional-values)
  (define arity (length positional-values))
  (cond
    [(<= arity 1) "true"]
    [(= arity 2)
     (format "(~a ~a ~a)" (first positional-values) operator (second positional-values))]
    [else
     (format "(~a)"
             (string-join
              (for/list ([left (in-list positional-values)]
                         [right (in-list (cdr positional-values))])
                (format "~a ~a ~a" left operator right))
              " && "))]))

(define (emit-discarded-if-expression test then else)
  (define lowered-test (lower-expr test))
  (define then-lines (emit-expression-statement then))
  (define else-lines (emit-expression-statement else))
  (append
   (lowered-statements lowered-test)
   (list (format "if ~a {" (lowered-value lowered-test)))
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
  (lowered
   (append (lowered-statements lowered-test)
           (lowered-statements lowered-then)
           (lowered-statements lowered-else))
   (format "if ~a { ~a } else { ~a }"
           (lowered-value lowered-test)
           (lowered-value lowered-then)
           (lowered-value lowered-else))))

(define (emit-let-expression bindings body)
  (define binding-lines
    (append-map
     (lambda (one-binding)
       (match one-binding
         [(binding name value)
          (emit-assignment name value)]))
     bindings))
  (lowered '()
           (format "{ ~a }"
                   (string-join
                    (append binding-lines
                            (emit-tail-expression (last body)))
                    " "))))

(define (emit-lambda-expression params body)
  (lowered
   '()
   (format "|~a| { ~a }"
           (string-join (map rust-name params) ", ")
           (string-join (emit-body body) " "))))

(define (emit-begin-expression exprs)
  (lowered '()
           (format "{ ~a }"
                   (string-join (emit-body exprs) " "))))

(define (emit-construct-expression type positional keyword)
  (when (pair? keyword)
    (error 'emit-rust-construct-expression
           "Rust constructors do not support keyword arguments: ~e"
           keyword))
  (define lowered-type (lower-expr type))
  (define-values (positional-statements positional-values)
    (lower-many positional))
  (lowered (append (lowered-statements lowered-type)
                   positional-statements)
           (format "~a(~a)"
                   (lowered-value lowered-type)
                   (string-join positional-values ", "))))

(define (lower-many exprs)
  (for/fold ([all-statements '()]
             [all-values '()])
            ([expr (in-list exprs)])
    (define lowered-expr (lower-expr expr))
    (values (append all-statements
                    (lowered-statements lowered-expr))
            (append all-values
                    (list (lowered-value lowered-expr))))))

(define (emit-literal value)
  (cond
    [(void? value) "()"]
    [(boolean? value) (if value "true" "false")]
    [(integer? value) (number->string value)]
    [(real? value) (format "~a" (exact->inexact value))]
    [(string? value) (format "String::from(~s)" value)]
    [(char? value) (format "'~a'" (escape-char value))]
    [(symbol? value) (format "String::from(~s)" (symbol->string value))]
    [(keyword? value) (format "String::from(~s)" (keyword->string value))]
    [(vector? value)
     (format "vec![~a]"
             (string-join
              (for/list ([item (in-vector value)])
                (emit-literal item))
              ", "))]
    [(hash? value)
     (format "std::collections::BTreeMap::from([~a])"
             (string-join
              (for/list ([entry (in-list (sort (hash->list value)
                                               string<?
                                               #:key (lambda (item)
                                                       (format "~v" (car item)))))])
                (format "(~a, ~a)"
                        (emit-literal (car entry))
                        (emit-literal (cdr entry))))
              ", "))]
    [(list? value)
     (format "vec![~a]"
             (string-join (map emit-literal value) ", "))]
    [else
     (error 'emit-rust-literal "unsupported literal value: ~e" value)]))

(define (indent-lines lines [level 1])
  (define prefix (make-string (* 4 level) #\space))
  (if (null? lines)
      (list (string-append prefix "()"))
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

(define (rust-name raw-name)
  (define sanitized (sanitize-identifier raw-name))
  (if (set-member? rust-keywords sanitized)
      (string-append sanitized "_")
      sanitized))

(define (rust-member-name raw-name)
  (sanitize-identifier raw-name))

(define (render-scope-ref parts)
  (when (null? parts)
    (error 'render-rust-scope-ref "scope reference needs at least one part"))
  (string-join (map rust-member-name parts) "::"))

(define (sanitize-identifier raw-name)
  (define input
    (cond
      [(symbol? raw-name) (symbol->string raw-name)]
      [(string? raw-name) raw-name]
      [else (error 'sanitize-rust-identifier
                   "identifier must be a string or symbol: ~e"
                   raw-name)]))
  (define normalized
    (regexp-replace* #px"[^A-Za-z0-9_]" (string-replace input "-" "_") "_"))
  (define collapsed
    (regexp-replace* #px"_+" normalized "_"))
  (cond
    [(string=? collapsed "") "value"]
    [(regexp-match? #px"^[0-9]" collapsed) (string-append "_" collapsed)]
    [else collapsed]))

(define (escape-char value)
  (cond
    [(char=? value #\newline) "\\n"]
    [(char=? value #\tab) "\\t"]
    [(char=? value #\return) "\\r"]
    [(char=? value #\') "\\'"]
    [(char=? value (integer->char 92)) "\\\\"]
    [else (string value)]))

(define (void-literal? expr)
  (and (literal? expr)
       (void? (literal-value expr))))
