#lang racket

;; Tests for behaviour specific to the Racket port of the wisp parser:
;; things Racket's reader does differently from Guile's, plus features
;; ported from newer upstream versions.

(require rackunit
         "../parse.rkt")

(define (parse str) (wisp-scheme-read-string str))

(test-case "escaped colon stays a symbol instead of becoming an inline colon"
  (check-equal? (parse "list 1 \\: 2\n") '((list 1 : 2)))
  (check-equal? (parse "list 1 |:| 2\n") '((list 1 : 2)))
  (check-equal? (parse "list 1 : 2\n") '((list 1 (2)))))

(test-case "escaped underscore is a plain symbol, even at line start"
  (check-equal? (parse "\\_ x\n") '((_ x)))
  (check-equal? (parse "foo\n___ bar\n") '((foo (bar)))))

(test-case "reading from a still-open pipe does not wait for more input"
  (define (read-chunk-with-timeout str)
    (define-values (in out) (make-pipe))
    (write-string str out)
    (flush-output out)
    (define ch (make-channel))
    (thread (λ () (channel-put ch (wisp-scheme-read-chunk in))))
    (sync/timeout 2 ch))
  (check-equal? (read-chunk-with-timeout "display 1\n\n\n") '((display 1)))
  (check-equal? (read-chunk-with-timeout "display 'x\n\n\n") '((display (quote x))))
  (check-equal? (read-chunk-with-timeout "foo .\n\n\n") '((foo))))

(test-case "lone prefix before the last datum on a line"
  (check-equal? (parse "foo ' bar\n") '((foo (quote bar))))
  (check-equal? (parse "foo ' ()\n") '((foo (quote ()))))
  (check-equal? (parse "display '\tfoo\n") '((display (quote foo)))))

(test-case "prefix tokens"
  (check-equal? (parse "foo #,@x\n") '((foo (unsyntax-splicing x))))
  (check-equal? (parse "foo\n  #,@ x\n") '((foo (unsyntax-splicing (x)))))
  (check-equal? (parse "display 'abcd\n") '((display (quote abcd)))))

(test-case "CRLF line endings"
  (check-equal? (parse "define : f x\r\n  * x x\r\n") '((define (f x) (* x x))))
  (check-equal? (parse "foo bar .\r\n") '((foo bar))))

(test-case "literal vectors with ##"
  (check-equal? (parse "## a b\n") '(#(a b)))
  (check-equal? (parse "##\n  a b\n") '(#((a b)))))

(test-case "a single space of indentation on the first line is ignored"
  (check-equal? (parse " foo bar\n") '((foo bar)))
  (check-exn #rx"wisp-syntax-error: The first symbol in a chunk must start at zero indentation"
             (λ () (parse "  foo bar\n"))))

(test-case "curly infix"
  (check-equal? (parse "display {1 + 2}\n") '((display (+ 1 2))))
  (check-equal? (parse "display {1 + 2 + 3}\n") '((display (+ 1 2 3))))
  (check-equal? (parse "display {{1 + 2} * 3}\n") '((display (* (+ 1 2) 3))))
  (check-equal? (parse "display {x}\n") '((display x)))
  (check-equal? (parse "display {}\n") '((display ())))
  (check-equal? (parse "display {f x}\n") '((display (f x))))
  (check-equal? (parse "display {1 + 2 * 3}\n") '((display ($nfx$ 1 + 2 * 3))))
  (check-equal? (parse "display '{a b c d}\n") '((display (quote ($nfx$ a b c d))))))

(test-case "#!curly-infix directive is accepted and produces nothing"
  (check-equal? (parse ". #!curly-infix\n\ndisplay {1 + 2}\n") '((display (+ 1 2))))
  (check-equal? (parse "foo #!curly-infix bar\n") '((foo bar))))

(test-case "a #lang line is skipped when a module file is read as data"
  (check-equal? (parse "#lang wisp\n\ndefine : f x\n  * x x\n") '((define (f x) (* x x))))
  (check-equal? (parse "#lang wisp\nfoo\n") '((foo))))

(test-case "error for an undefined lower indentation level is a warning, not a failure"
  (define warning (open-output-string))
  (define result
    (parameterize ((current-error-port warning))
      (parse "foo\n    bar\n  baz\n")))
  (check-equal? result '((foo (bar) (baz))))
  (check-regexp-match #rx"WARNING: used lower but undefined indentation level" (get-output-string warning)))

(test-case "wisp-read-syntax gives every top-level form the location of its chunk"
  (define forms (wisp-read-syntax 'test (open-input-string "define : f x\n  * x x\n\ndisplay 1\n")))
  (check-equal? (map syntax->datum forms) '((define (f x) (* x x)) (display 1)))
  (check-equal? (map syntax-line forms) '(1 4)))
