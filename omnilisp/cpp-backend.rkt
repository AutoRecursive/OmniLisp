#lang racket

(require racket/list
         racket/match
         racket/set
         racket/string
         "ir.rkt")

(provide emit-program)

(struct lowered (statements value) #:transparent)

(define current-name-counter (make-parameter 0))

(define cpp-keywords
  (set "alignas" "alignof" "and" "and_eq" "asm" "auto" "bitand" "bitor"
       "bool" "break" "case" "catch" "char" "char8_t" "char16_t" "char32_t"
       "class" "compl" "concept" "const" "consteval" "constexpr" "constinit"
       "continue" "co_await" "co_return" "co_yield" "decltype" "default"
       "delete" "do" "double" "dynamic_cast" "else" "enum" "explicit" "export"
       "extern" "false" "float" "for" "friend" "goto" "if" "inline" "int"
       "long" "mutable" "namespace" "new" "noexcept" "not" "not_eq" "nullptr"
       "operator" "or" "or_eq" "private" "protected" "public" "register"
       "reinterpret_cast" "requires" "return" "short" "signed" "sizeof" "static"
       "static_assert" "static_cast" "struct" "switch" "template" "this"
       "thread_local" "throw" "true" "try" "typedef" "typeid" "typename"
       "union" "unsigned" "using" "virtual" "void" "volatile" "wchar_t"
       "while" "xor" "xor_eq"))

(define (emit-program prog)
  (parameterize ([current-name-counter 0])
    (define-values (function-blocks main-blocks include-lines import-comments)
      (partition-top-level (program-forms prog)))
    (define blocks
      (append
       (list (append (runtime-include-lines)
                     include-lines))
       (list (runtime-helper-lines))
       (if (null? import-comments) '() (list import-comments))
       function-blocks
       (list (append (list "int main() {")
                     (indent-lines (append main-blocks
                                           (list "return 0;")))
                     (list "}")))))
    (string-join (join-blocks blocks) "\n")))

(define (runtime-include-lines)
  (list "#include <iostream>"
        "#include <map>"
        "#include <string>"
        "#include <utility>"
        "#include <vector>"))

(define (runtime-helper-lines)
  (list
        "template <typename T>"
        "std::ostream& operator<<(std::ostream& out, const std::vector<T>& values) {"
        "    out << \"[\";"
        "    for (std::size_t i = 0; i < values.size(); ++i) {"
        "        if (i != 0) out << \", \";"
        "        out << values[i];"
        "    }"
        "    out << \"]\";"
        "    return out;"
        "}"
        ""
        "template <typename K, typename V>"
        "std::ostream& operator<<(std::ostream& out, const std::map<K, V>& values) {"
        "    out << \"{\";"
        "    bool first = true;"
        "    for (const auto& [key, value] : values) {"
        "        if (!first) out << \", \";"
        "        first = false;"
        "        out << key << \": \" << value;"
        "    }"
        "    out << \"}\";"
        "    return out;"
        "}"))

(define (partition-top-level forms)
  (for/fold ([function-blocks '()]
             [main-lines '()]
             [include-lines '()]
             [import-comments '()])
            ([form (in-list forms)])
    (match form
      [(include-spec target header style)
       (unless (eq? target 'cpp)
         (error 'emit-cpp-include
                "C++ backend cannot emit include for target ~e"
                target))
       (values function-blocks
               main-lines
               (append include-lines
                       (list (format "#include ~a"
                                     (render-include-header header style))))
               import-comments)]
      [(import-spec module alias)
       (values function-blocks
               main-lines
               include-lines
               (append import-comments
                       (list (format "// import ~a~a"
                                     module
                                     (if alias
                                         (format " as ~a" (cpp-name alias))
                                         "")))))]
      [(from-import-spec module names)
       (values function-blocks
               main-lines
               include-lines
               (append import-comments
                       (list (format "// from ~a import ~a"
                                     module
                                     (string-join
                                      (for/list ([name (in-list names)])
                                        (match name
                                          [(import-name imported alias)
                                           (if alias
                                               (format "~a as ~a" imported (cpp-name alias))
                                               imported)]))
                                      ", ")))))]
      [(define-function _ _ _)
       (values (append function-blocks
                       (list (emit-top-level-function form)))
               main-lines
               include-lines
               import-comments)]
      [(define-value name expr)
       (values function-blocks
               (append main-lines (emit-assignment name expr))
               include-lines
               import-comments)]
      [(expr-stmt expr)
       (values function-blocks
               (append main-lines (emit-expression-statement expr))
               include-lines
               import-comments)])))

(define (render-include-header header style)
  (match style
    ['system (format "<~a>" header)]
    ['local (format "~s" header)]
    [_ (error 'render-include-header "unknown include style: ~e" style)]))

(define (emit-top-level-function form)
  (match form
    [(define-function name params body)
     (append
      (list (format "auto ~a(~a) {"
                    (cpp-name name)
                    (string-join
                     (for/list ([param (in-list params)])
                       (format "auto ~a" (cpp-name param)))
                     ", ")))
      (indent-lines (emit-body body))
      (list "}"))]))

(define (emit-assignment name expr)
  (define lowered-expr (lower-expr expr))
  (append (lowered-statements lowered-expr)
          (list (format "auto ~a = ~a;"
                        (cpp-name name)
                        (lowered-value lowered-expr)))))

(define (emit-body exprs)
  (cond
    [(null? exprs) (list "return nullptr;")]
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
     (lowered '() (cpp-name name))]
    [(attr-expr target name)
     (define lowered-target (lower-expr target))
     (lowered (lowered-statements lowered-target)
              (format "~a.~a"
                      (lowered-value lowered-target)
                      (cpp-member-name name)))]
    [(scope-ref parts)
     (lowered '() (render-scope-ref parts))]
    [(construct-expr type positional keyword)
     (emit-construct-expression type positional keyword)]
    [(list-expr elements)
     (define-values (statements values) (lower-many elements))
     (lowered statements
              (format "std::vector{~a}" (string-join values ", ")))]
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
                            (list (format "std::pair{~a, ~a}"
                                          (lowered-value lowered-key)
                                          (lowered-value lowered-value*)))))])))
     (lowered statements
              (format "std::map{~a}" (string-join pieces ", ")))]
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
    (error 'emit-cpp-application
           "C++ backend does not support keyword arguments yet: ~e"
           keyword))
  (define lowered-target (lower-expr target))
  (define-values (positional-statements positional-values)
    (lower-many positional))
  (define setup
    (append (lowered-statements lowered-target)
            positional-statements))
  (lowered setup
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

(define (emit-construct-expression type positional keyword)
  (when (pair? keyword)
    (error 'emit-cpp-construct-expression
           "C++ constructors do not support keyword arguments: ~e"
           keyword))
  (define lowered-type (lower-expr type))
  (define-values (positional-statements positional-values)
    (lower-many positional))
  (lowered (append (lowered-statements lowered-type)
                   positional-statements)
           (format "~a(~a)"
                   (lowered-value lowered-type)
                   (string-join positional-values ", "))))

(define (render-special-call name positional-values)
  (cond
    [(operator-name? name)
     (render-operator-call name positional-values)]
    [(or (string=? name "display")
         (string=? name "displayln")
         (string=? name "print"))
     (format "(std::cout~a << std::endl)"
             (apply string-append
                    (for/list ([value (in-list positional-values)])
                      (format " << ~a" value))))]
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
       [(zero? arity) (error 'render-cpp-operator-call "- expects at least one argument")]
       [(= arity 1) (format "(-~a)" (car positional-values))]
       [else (format "(~a)" (string-join positional-values " - "))])]
    [(string=? name "/")
     (cond
       [(zero? arity) (error 'render-cpp-operator-call "/ expects at least one argument")]
       [(= arity 1) (format "(1 / ~a)" (car positional-values))]
       [else (format "(~a)" (string-join positional-values " / "))])]
    [(string=? name "%")
     (cond
       [(< arity 2)
        (error 'render-cpp-operator-call "% expects at least two arguments")]
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
       (error 'render-cpp-operator-call "not expects exactly one argument"))
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
               (format "((~a) ? (~a) : (~a))"
                       (lowered-value lowered-test)
                       (lowered-value lowered-then)
                       (lowered-value lowered-else)))
      (let ([temp-name (fresh-name "__omni_if_value")])
        (lowered
         (append
          (lowered-statements lowered-test)
          (list (format "auto ~a = decltype(~a){};"
                        temp-name
                        (lowered-value lowered-then))
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
    (list (format "auto ~a = [&]() {" helper-name))
    (indent-lines (append binding-lines
                          (emit-body body)))
    (list "};"))
   (format "~a()" helper-name)))

(define (emit-lambda-expression params body)
  (define lowered-body (lower-expr (last body)))
  (when (or (pair? (drop-right body 1))
            (pair? (lowered-statements lowered-body)))
    (error 'emit-cpp-lambda-expression
           "C++ backend only supports single-expression lambdas for now"))
  (lowered
   '()
   (format "[&](~a) { return ~a; }"
           (string-join
            (for/list ([param (in-list params)])
              (format "auto ~a" (cpp-name param)))
            ", ")
           (lowered-value lowered-body))))

(define (emit-begin-expression exprs)
  (define helper-name (fresh-name "__omni_begin"))
  (lowered
   (append
    (list (format "auto ~a = [&]() {" helper-name))
    (indent-lines (emit-body exprs))
    (list "};"))
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

(define (emit-literal value)
  (cond
    [(void? value) "nullptr"]
    [(boolean? value) (if value "true" "false")]
    [(integer? value) (number->string value)]
    [(real? value) (format "~a" (exact->inexact value))]
    [(string? value) (format "std::string{~s}" value)]
    [(char? value) (format "'~a'" (escape-char value))]
    [(symbol? value) (format "std::string{~s}" (symbol->string value))]
    [(keyword? value) (format "std::string{~s}" (keyword->string value))]
    [(vector? value)
     (format "std::vector{~a}"
             (string-join
              (for/list ([item (in-vector value)])
                (emit-literal item))
              ", "))]
    [(hash? value)
     (format "std::map{~a}"
             (string-join
              (for/list ([entry (in-list (sort (hash->list value)
                                               string<?
                                               #:key (lambda (item)
                                                       (format "~v" (car item)))))])
                (format "std::pair{~a, ~a}"
                        (emit-literal (car entry))
                        (emit-literal (cdr entry))))
              ", "))]
    [(list? value)
     (format "std::vector{~a}"
             (string-join (map emit-literal value) ", "))]
    [else
     (error 'emit-cpp-literal "unsupported literal value: ~e" value)]))

(define (indent-lines lines [level 1])
  (define prefix (make-string (* 4 level) #\space))
  (if (null? lines)
      (list (string-append prefix "/* no-op */"))
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

(define (cpp-name raw-name)
  (define sanitized (sanitize-identifier raw-name))
  (if (set-member? cpp-keywords sanitized)
      (string-append sanitized "_")
      sanitized))

(define (cpp-member-name raw-name)
  (sanitize-identifier raw-name))

(define (render-scope-ref parts)
  (when (null? parts)
    (error 'render-scope-ref "scope reference needs at least one part"))
  (string-join (map cpp-member-name parts) "::"))

(define (sanitize-identifier raw-name)
  (define input
    (cond
      [(symbol? raw-name) (symbol->string raw-name)]
      [(string? raw-name) raw-name]
      [else (error 'sanitize-cpp-identifier
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
