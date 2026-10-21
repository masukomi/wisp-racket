#lang racket

;; Tests for racket2wisp.w: every Racket file in the repository and every
;; upstream .scm fixture must survive a round trip (convert to wisp, parse
;; with the wisp reader, compare with the original forms), with and without
;; comments. The exact-output cases pin down the idioms the emitter chooses.

(require rackunit
         racket/runtime-path
         "../parse.rkt"
         "../racket2wisp.w")

(define-runtime-path here ".")
(define-runtime-path root "..")

;; ---------------------------------------------------------------------
;; round trips

(define (original-forms path)
  (define src (file->string path))
  (cond
    [(regexp-match? #rx"^#lang " src)
     (define in (open-input-string src))
     (port-count-lines! in)
     (define stx (parameterize ([read-accept-reader #t] [read-accept-lang #t])
                   (read-syntax 'original in)))
     (match (syntax->datum stx)
       [(list 'module _ _ (list '#%module-begin forms ...)) forms])]
    [else
     (call-with-input-file path
       (λ (in)
         (let loop ((acc '()))
           (define next (read-with-curly-infix in))
           (if (eof-object? next) (reverse acc) (loop (cons next acc))))))]))

(define (round-trip path comments?)
  (define wisp (file->wisp path #:comments? comments?))
  (wisp-scheme-read-string wisp))

(define racket-files
  (append (for/list ([f '("main.rkt" "parse.rkt")]) (build-path root f))
          (for/list ([f (directory-list here)]
                     #:when (path-has-extension? f #".rkt"))
            (build-path here f))))

(define scm-files
  (for/list ([f (directory-list (build-path here "upstream"))]
             #:when (path-has-extension? f #".scm"))
    (build-path here "upstream" f)))

(for ([path (append racket-files scm-files)])
  (define name (path->string (file-name-from-path path)))
  (define expected (original-forms path))
  (test-case (string-append name " round trip, with comments")
    (check-equal? (round-trip path #t) expected))
  (test-case (string-append name " round trip, no comments")
    (check-equal? (round-trip path #f) expected)))

;; ---------------------------------------------------------------------
;; the two front ends agree once comments and blanks are ignored

(define (strip node)
  (cond
    [(lst? node)
     (lst (lst-opener node)
          (map strip (filter-not (λ (n) (or (comment? n) (blank? n))) (lst-items node)))
          (and (lst-tail node) (strip (lst-tail node)))
          (lst-prefix node)
          'auto)]
    [(atom? node) (atom-datum node)]
    [else node]))

(test-case "front ends agree on parse.rkt"
  (define src (file->string (build-path root "parse.rkt")))
  (define lexed (lex->ir (open-input-string src)))
  (define read (read-syntax->ir (open-input-string src)))
  (check-equal? (wisp-file-lang lexed) "racket")
  (check-equal? (wisp-file-lang read) "racket")
  (check-equal? (map strip (filter lst? (wisp-file-nodes lexed)))
                (map strip (filter lst? (wisp-file-nodes read)))))

;; ---------------------------------------------------------------------
;; exact output

(define (convert str) (racket->wisp str))

(test-case "define with a body uses the colon idiom"
  (check-equal? (convert "#lang racket\n(define (f x)\n  (* x x)\n  (+ x 1))\n")
                "#lang wisp\n\ndefine : f x\n  * x x\n  + x 1\n"))

(test-case "forms on one source line stay on one line; source blank lines are kept"
  (check-equal? (convert "(define x 1)\n(f (g 1) 2)\n")
                "define x 1\nf (g 1) 2\n")
  (check-equal? (convert "(define x 1)\n\n(f (g 1) 2)\n")
                "define x 1\n\nf (g 1) 2\n"))

(test-case "a trailing call on one line uses the inline colon"
  (check-equal? (convert "(display (string-append \"Hello \" name))\n")
                "display : string-append \"Hello \" name\n")
  (check-equal? (convert "(f (g 1) 2)\n") "f (g 1) 2\n"))

(test-case "let bindings: a list of lists becomes a colon block"
  (check-equal? (convert "(let ([x 1])\n  (display x)\n  (newline))\n")
                "let\n  : x 1\n  display x\n  newline\n")
  (check-equal? (convert "(let ([x 1] [y 2])\n  (display x))\n")
                "let\n  : x 1\n    y 2\n  display x\n")
  (check-equal? (convert "(let loop ([x 1])\n  (loop x))\n")
                "let loop\n  : x 1\n  loop x\n"))

(test-case "the no-comments front end guesses layout from the shape"
  (check-equal? (racket->wisp "(define (f x) (* x x) (+ x 1))\n" #:comments? #f)
                "define : f x\n  * x x\n  + x 1\n")
  (check-equal? (racket->wisp "(define x (f 1))\n" #:comments? #f)
                "define x : f 1\n"))

(test-case "cond clauses are lines headed by an inline test"
  (check-equal? (convert "(define (sign x)\n  (cond [(< x 0) 'neg]\n        [else 'pos]))\n")
                "define : sign x\n  cond\n    (< x 0) 'neg\n    else 'pos\n"))

(test-case "a list-headed form that does not fit opens with a colon"
  (parameterize ([current-line-width 20])
    (check-equal? (convert "((lambda (x) (display x) x) some-long-argument)\n")
                  ":\n  lambda : x\n    display x\n    . x\n  . some-long-argument\n")))

(test-case "quoted data: inline when short, prefix line when long"
  (check-equal? (convert "(f '(a b) 'c)\n") "f '(a b) 'c\n")
  (parameterize ([current-line-width 20])
    (check-equal? (convert "'(aaaa bbbb cccc)\n") "' aaaa bbbb cccc\n")))

(test-case "a quoted list too long for one line expands to quote"
  (parameterize ([current-line-width 16])
    (check-equal? (convert "'((aaaa bbbb) (cccc dddd))\n")
                  "quote\n  : aaaa bbbb\n    cccc dddd\n")))

(test-case "dotted pairs, vectors and empty lists are written inline"
  (check-equal? (convert "(f (a . b) #(1 2) '())\n") "f (a . b) #(1 2) '()\n")
  (parameterize ([current-line-width 14])
    (check-equal? (convert "(define pair (aaaa . bbbb))\n") "define pair\n  . (aaaa . bbbb)\n")))

(test-case "a bare atom at top level needs a leading dot"
  (check-equal? (convert "x\n") ". x\n"))

(test-case "curly braces pass through for data files and become parens in modules"
  (check-equal? (convert "(display {1 + 2})\n") "display {1 + 2}\n")
  (check-equal? (convert "#lang racket\n(display {1 + 2})\n") "#lang wisp\n\ndisplay : 1 + 2\n"))

(test-case "colon and leading underscore are escaped"
  (check-equal? (convert "(f : 1)\n(_x 2)\n") "f \\: 1\n\\_x 2\n")
  (check-equal? (convert "(f _x)\n") "f _x\n")
  (check-equal? (wisp-scheme-read-string (convert "(f : 1)\n(_x 2)\n")) '((f : 1) (_x 2))))

(test-case "trailing comments stay on their line and push the rest down"
  (check-equal? (convert "(f a ; about a\n   b)\n")
                "f a ; about a\n  . b\n")
  (check-equal? (convert "(define x ; why\n  1)\n")
                "define x ; why\n  . 1\n"))

(test-case "own-line comments keep their place inside a form"
  (check-equal? (convert ";; header\n(define (f)\n  ;; body\n  1)\n")
                ";; header\ndefine : f\n  ;; body\n  . 1\n"))

(test-case "datum comments become line comments, since #; cannot end a wisp line"
  (check-equal? (convert "(f #;(old) new)\n") "f ; #;(old)\n  . new\n")
  (check-equal? (convert "(f\n  #;(old\n     one)\n  new)\n") "f\n  ; #;(old\n;      one)\n  . new\n")
  (check-equal? (wisp-scheme-read-string (convert "(f #;(old) new)\n")) '((f new))))

(test-case "block comments survive"
  (check-equal? (convert "(f #| note |# a)\n") "f #| note |#\n  . a\n")
  (check-equal? (convert "#| top\n   note |#\n(f a)\n") "#| top\n   note |#\nf a\n"))

(test-case "blank lines inside a form are capped at one, at top level at two"
  (check-equal? (convert "(define (f)\n  a\n\n\n\n  b)\n") "define : f\n  . a\n\n  . b\n")
  (check-equal? (convert "a\n\n\n\n\nb\n") ". a\n\n\n. b\n"))

(test-case "multi-line strings are kept verbatim"
  (check-equal? (convert "(display \"one\ntwo\")\n") "display \"one\ntwo\"\n")
  (check-equal? (wisp-scheme-read-string (convert "(display \"one\ntwo\")\n")) '((display "one\ntwo"))))

(test-case "a non-racket #lang is rewritten with a warning"
  (define err (open-output-string))
  (define out (parameterize ([current-error-port err]) (convert "#lang racket/base\n(define x 1)\n")))
  (check-equal? out "#lang wisp\n\ndefine x 1\n")
  (check-regexp-match #rx"racket/base" (get-output-string err)))

(test-case "comments are preserved for parse.rkt"
  (define src (file->string (build-path root "parse.rkt")))
  (define out (racket->wisp src))
  (for ([line (string-split src "\n")]
        #:when (regexp-match? #px"^\\s*;" line))
    (check-true (string-contains? out (string-trim line)) line)))

(test-case "unbalanced input is an error"
  (check-exn #rx"unclosed paren" (λ () (convert "(f a")))
  (check-exn #rx"unbalanced closing paren" (λ () (convert "f a)"))))
