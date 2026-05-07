#lang racket

(require racket/cmdline
         racket/file
         racket/pretty
         "transpiler.rkt")

(module+ main
  (define dump-ir? #f)
  (define list-targets? #f)
  (define output-path #f)
  (define target "python")
  (define input-paths
    (command-line
     #:program "omnilisp"
     #:once-each
     [("--dump-ir") "Print normalized IR to stderr before generating target code."
      (set! dump-ir? #t)]
     [("--list-targets") "List available target backends and exit."
      (set! list-targets? #t)]
     [("-t" "--target") target-name "Emit code for the given target backend."
      (set! target target-name)]
     [("-o" "--output") path "Write generated code to the given file."
      (set! output-path path)]
     #:args paths
     paths))

  (when list-targets?
    (for ([backend (in-list (available-backends))])
      (displayln (format "~a\t~a"
                         (backend-id backend)
                         (backend-description backend))))
    (exit 0))

  (define input-path
    (cond
      [(null? input-paths)
       (error 'omnilisp "an input file is required unless --list-targets is used")]
      [(null? (cdr input-paths))
       (car input-paths)]
      [else
       (error 'omnilisp "expected exactly one input file, got ~a" (length input-paths))]))

  (define ir (parse-source-file input-path))
  (when dump-ir?
    (pretty-write ir (current-error-port)))

  (define output-code (transpile-program ir #:target target))
  (cond
    [output-path
     (display-to-file output-code output-path #:exists 'replace)]
    [else
     (displayln output-code)]))
