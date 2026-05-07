#lang racket

(require racket/list
         racket/match
         racket/port
         racket/string
         "ir.rkt")

(provide parse-file
         parse-port
         parse-string)

(define (parse-file path)
  (call-with-input-file path
    (lambda (in)
      (parse-port in #:source path))))

(define (parse-string source [source-name 'string])
  (parse-port (open-input-string source) #:source source-name))

(define (parse-port in #:source [source-name #f])
  (program
   (append-map parse-top-level-form
               (unwrap-module-datums (read-all-datums in source-name)))))

(define (read-all-datums in source-name)
  (parameterize ([read-accept-reader #t])
    (let loop ([acc '()])
      (define next-datum (read-syntax source-name in))
      (if (eof-object? next-datum)
          (reverse acc)
          (loop (cons (syntax->datum next-datum) acc))))))

(define (unwrap-module-datums datums)
  (match datums
    [(list `(module ,_ ,_ (#%module-begin ,body ...))) body]
    [(list `(module* ,_ ,_ (#%module-begin ,body ...))) body]
    [other other]))

(define (parse-top-level-form datum)
  (match datum
    [`(cpp-include ,headers ...)
     (map parse-cpp-include headers)]
    [`(rust-use ,paths ...)
     (map parse-rust-use paths)]
    [`(py-import ,specs ...)
     (map parse-import-spec specs)]
    [`(import ,specs ...)
     (map parse-import-spec specs)]
    [`(define (,name ,params ...) ,body ...)
     (list (define-function (symbol->string name)
                            (map parse-parameter params)
                            (parse-body body 'define-function)))]
    [`(define ,name ,expr)
     (unless (symbol? name)
       (error 'parse-top-level-form "define name must be a symbol: ~e" datum))
     (list (define-value (symbol->string name)
                         (parse-expr expr)))]
    [`(begin ,forms ...)
     (append-map parse-top-level-form forms)]
    [_
     (list (expr-stmt (parse-expr datum)))]))

(define (parse-import-spec spec)
  (match spec
    [(? symbol? module-name)
     (import-spec (module-name->string module-name) #f)]
    [(? string? module-name)
     (import-spec module-name #f)]
    [`(,module-name ,alias)
     #:when (or (symbol? module-name) (string? module-name))
     (unless (symbol? alias)
       (error 'parse-import-spec "import alias must be a symbol: ~e" spec))
     (import-spec (module-name->string module-name)
                  (symbol->string alias))]
    [`(:from ,module-name ,names ...)
     (from-import-spec (module-name->string module-name)
                       (map parse-import-name names))]
    [_
     (error 'parse-import-spec "unsupported import form: ~e" spec)]))

(define (parse-import-name datum)
  (match datum
    [(? symbol? name)
     (import-name (symbol->string name) #f)]
    [`(,name ,alias)
     #:when (and (symbol? name) (symbol? alias))
     (import-name (symbol->string name)
                  (symbol->string alias))]
    [_
     (error 'parse-import-name "unsupported from-import name: ~e" datum)]))

(define (parse-cpp-include datum)
  (match datum
    [`(:local ,header)
     (include-spec 'cpp (include-header->string header) 'local)]
    [_
     (include-spec 'cpp (include-header->string datum) 'system)]))

(define (parse-rust-use datum)
  (match datum
    [`(:as ,path ,alias)
     (include-spec 'rust
                   (format "~a as ~a"
                           (rust-path->string path)
                           (scope-part->string alias))
                   'use)]
    [_
     (include-spec 'rust (rust-path->string datum) 'use)]))

(define (include-header->string datum)
  (define raw
    (cond
      [(symbol? datum) (symbol->string datum)]
      [(string? datum) datum]
      [else (error 'include-header->string
                   "include header must be a symbol or string: ~e"
                   datum)]))
  (cond
    [(and (string-prefix? raw "<")
          (string-suffix? raw ">"))
     (substring raw 1 (sub1 (string-length raw)))]
    [(and (string-prefix? raw "\"")
          (string-suffix? raw "\""))
     (substring raw 1 (sub1 (string-length raw)))]
    [else raw]))

(define (parse-parameter param)
  (unless (symbol? param)
    (error 'parse-parameter "function parameter must be a symbol: ~e" param))
  (symbol->string param))

(define (parse-body body who)
  (when (null? body)
    (error who "expected at least one body expression"))
  (map parse-expr body))

(define (parse-expr datum)
  (cond
    [(literal-datum? datum) (literal datum)]
    [(symbol? datum) (parse-reference-symbol datum)]
    [(pair? datum)
     (match datum
       [`(quote ,value)
        (literal value)]
       [`(if ,test ,then ,else)
        (if-expr (parse-expr test)
                 (parse-expr then)
                 (parse-expr else))]
       [`(when ,test ,body ...)
        (if-expr (parse-expr test)
                 (make-sequence-expr body 'when)
                 (literal (void)))]
       [`(unless ,test ,body ...)
        (if-expr (application (var-ref "not")
                              (list (parse-expr test))
                              '())
                 (make-sequence-expr body 'unless)
                 (literal (void)))]
       [`(cond ,clauses ...)
        (parse-cond clauses)]
       [`(and ,exprs ...)
        (parse-and exprs)]
       [`(or ,exprs ...)
        (parse-or exprs)]
       [`(let (,bindings ...) ,body ...)
        (let-expr (map parse-binding bindings)
                  (parse-body body 'let))]
       [`(let* (,bindings ...) ,body ...)
        (parse-let* bindings body)]
       [`(case ,key-expr ,clauses ...)
        (parse-case key-expr clauses)]
       [`(lambda (,params ...) ,body ...)
        (lambda-expr (map parse-parameter params)
                     (parse-body body 'lambda))]
       [`(attr ,target ,field)
        (parse-attr target field)]
       [`(py-attr ,target ,field)
        (parse-attr target field)]
       [`(py-call ,target ,args ...)
        (define-values (positional keyword) (parse-call-arguments args))
        (application (parse-expr target) positional keyword)]
       [`(cpp-call ,target ,args ...)
        (define-values (positional keyword) (parse-call-arguments args))
        (application (parse-expr target) positional keyword)]
       [`(rust-call ,target ,args ...)
        (define-values (positional keyword) (parse-call-arguments args))
        (application (parse-expr target) positional keyword)]
       [`(cpp-scope ,parts ...)
        (scope-ref (map scope-part->string parts))]
       [`(rust-path ,parts ...)
        (scope-ref (map scope-part->string parts))]
       [`(:: ,parts ...)
        (scope-ref (map scope-part->string parts))]
       [`(cpp-construct ,type ,args ...)
        (define-values (positional keyword) (parse-call-arguments args))
        (construct-expr (parse-type-ref type) positional keyword)]
       [`(cpp-new ,type ,args ...)
        (define-values (positional keyword) (parse-call-arguments args))
        (construct-expr (parse-type-ref type) positional keyword)]
       [`(rust-construct ,type ,args ...)
        (define-values (positional keyword) (parse-call-arguments args))
        (construct-expr (parse-type-ref type) positional keyword)]
       [`(list ,elements ...)
        (list-expr (map parse-expr elements))]
       [`(hash ,entries ...)
        (parse-hash-expr entries)]
       [`(begin ,exprs ...)
        (begin-expr (parse-body exprs 'begin))]
       [`(quote-syntax ,_)
        (error 'parse-expr "quote-syntax is not supported by the transpiler")]
       [`(,target ,args ...)
        (define-values (positional keyword) (parse-call-arguments args))
        (application (parse-expr target) positional keyword)])]
    [else
     (error 'parse-expr "unsupported datum: ~e" datum)]))

(define (make-sequence-expr datums who)
  (cond
    [(null? datums) (literal (void))]
    [(null? (cdr datums)) (parse-expr (car datums))]
    [else (begin-expr (parse-body datums who))]))

(define (parse-cond clauses)
  (let loop ([rest clauses])
    (cond
      [(null? rest) (literal (void))]
      [else
       (match (car rest)
         [`(else ,body ...)
          (unless (null? (cdr rest))
            (error 'parse-cond "else clause must be the last cond clause: ~e" clauses))
          (make-sequence-expr body 'cond)]
         [`(,test ,body ...)
          (if-expr (parse-expr test)
                   (if (null? body)
                       (parse-expr test)
                       (make-sequence-expr body 'cond))
                   (loop (cdr rest)))]
         [_
          (error 'parse-cond "unsupported cond clause: ~e" (car rest))])])))

(define (parse-and exprs)
  (cond
    [(null? exprs) (literal #t)]
    [(null? (cdr exprs)) (parse-expr (car exprs))]
    [else
     (if-expr (parse-expr (car exprs))
              (parse-and (cdr exprs))
              (literal #f))]))

(define (parse-or exprs)
  (cond
    [(null? exprs) (literal #f)]
    [(null? (cdr exprs)) (parse-expr (car exprs))]
    [else
     (let ([temp-name (gensym-string "or-temp")])
       (let-expr (list (binding temp-name (parse-expr (car exprs))))
                 (list (if-expr (var-ref temp-name)
                                (var-ref temp-name)
                                (parse-or (cdr exprs))))))]))

(define (parse-let* bindings body)
  (cond
    [(null? bindings)
     (make-sequence-expr body 'let*)]
    [else
     (let-expr (list (parse-binding (car bindings)))
               (list (parse-let* (cdr bindings) body)))]))

(define (parse-case key-expr clauses)
  (let ([key-name (gensym-string "case-key")])
    (let-expr (list (binding key-name (parse-expr key-expr)))
              (list (parse-case-clauses (var-ref key-name) clauses)))))

(define (parse-case-clauses key-ref clauses)
  (let loop ([rest clauses])
    (cond
      [(null? rest) (literal (void))]
      [else
       (match (car rest)
         [`(else ,body ...)
          (unless (null? (cdr rest))
            (error 'parse-case "else clause must be the last case clause"))
          (make-sequence-expr body 'case)]
         [`((,values ...) ,body ...)
          (if-expr (parse-case-test key-ref values)
                   (make-sequence-expr body 'case)
                   (loop (cdr rest)))]
         [_
          (error 'parse-case "unsupported case clause: ~e" (car rest))])])))

(define (parse-case-test key-ref values)
  (cond
    [(null? values) (literal #f)]
    [(null? (cdr values))
     (application (var-ref "equal?")
                  (list key-ref (parse-expr (car values)))
                  '())]
    [else
     (if-expr (application (var-ref "equal?")
                           (list key-ref (parse-expr (car values)))
                           '())
              (literal #t)
              (parse-case-test key-ref (cdr values)))]))

(define (gensym-string prefix)
  (string-append prefix (number->string (current-inexact-milliseconds))))

(define (parse-binding datum)
  (match datum
    [`(,name ,value)
     #:when (symbol? name)
     (binding (symbol->string name)
              (parse-expr value))]
    [_
     (error 'parse-binding "unsupported let binding: ~e" datum)]))

(define (scope-part->string datum)
  (unless (symbol? datum)
    (error 'scope-part->string "scope parts must be symbols: ~e" datum))
  (symbol->string datum))

(define (parse-type-ref datum)
  (cond
    [(symbol? datum)
     (scope-ref (map scope-part->string
                     (string->symbol-parts (symbol->string datum))))]
    [(and (pair? datum)
          (memq (car datum) '(cpp-scope rust-path ::)))
     (match datum
       [`(,_ ,parts ...)
        (scope-ref (map scope-part->string parts))])]
    [else
     (error 'parse-type-ref "unsupported scoped type reference: ~e" datum)]))

(define (rust-path->string datum)
  (cond
    [(symbol? datum) (symbol->string datum)]
    [(string? datum) datum]
    [(and (pair? datum)
          (memq (car datum) '(rust-path ::)))
     (match datum
       [`(,_ ,parts ...)
        (string-join (map scope-part->string parts) "::")])]
    [else
     (error 'rust-path->string "unsupported Rust path: ~e" datum)]))

(define (string->symbol-parts value)
  (map string->symbol
       (filter (lambda (part) (not (string=? part "")))
               (string-split value "::"))))

(define (parse-attr target field)
  (unless (symbol? field)
    (error 'parse-attr "attribute name must be a symbol: ~e" field))
  (attr-expr (parse-expr target)
             (symbol->string field)))

(define (parse-hash-expr entries)
  (unless (even? (length entries))
    (error 'parse-hash-expr "hash expects an even number of key/value entries: ~e" entries))
  (dict-expr
   (for/list ([chunk (in-slice 2 entries)])
     (match chunk
       [(list key value)
        (dict-entry (parse-expr key)
                    (parse-expr value))]))))

(define (parse-call-arguments args)
  (let loop ([rest args]
             [positional '()]
             [keyword '()]
             [saw-keyword? #f])
    (cond
      [(null? rest)
       (values (reverse positional)
               (reverse keyword))]
      [(keyword? (car rest))
       (when (null? (cdr rest))
         (error 'parse-call-arguments "missing value for keyword argument ~a" (car rest)))
       (loop (cddr rest)
             positional
             (cons (keyword-arg (keyword->string (car rest))
                                (parse-expr (cadr rest)))
                   keyword)
             #t)]
      [saw-keyword?
       (error 'parse-call-arguments
              "positional arguments cannot appear after keyword arguments: ~e"
              args)]
      [else
       (loop (cdr rest)
             (cons (parse-expr (car rest)) positional)
             keyword
             #f)])))

(define (literal-datum? datum)
  (or (number? datum)
      (string? datum)
      (boolean? datum)
      (char? datum)
      (bytes? datum)
      (vector? datum)
      (hash? datum)
      (void? datum)
      (null? datum)))

(define (parse-reference-symbol sym)
  (define parts
    (filter (lambda (part) (not (string=? part "")))
            (string-split (symbol->string sym) ".")))
  (cond
    [(null? parts)
     (error 'parse-reference-symbol "empty symbol is not supported: ~e" sym)]
    [(= (length parts) 1)
     (var-ref (car parts))]
    [else
     (for/fold ([expr (var-ref (car parts))])
               ([part (in-list (cdr parts))])
       (attr-expr expr part))]))

(define (module-name->string value)
  (define raw
    (cond
      [(symbol? value) (symbol->string value)]
      [(string? value) value]
      [else (error 'module-name->string "module name must be a symbol or string: ~e" value)]))
  (string-replace raw "/" "."))
