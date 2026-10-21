#lang racket

;; Scheme-only implementation of a wisp-preprocessor which output a
;; scheme code tree to feed to a scheme interpreter instead of a
;; preprocessed file.

;; Ported from Guile's wisp-scheme.scm to Racket.

;; Limitations:
;; - only unescapes up to 6 leading underscores at line start (\______)
;; - source locations are only tracked per top-level form, not per
;;   sub-expression (Guile's source properties have no Racket equivalent
;;   for plain data).

(provide wisp-read-syntax
         read-with-curly-infix
         wisp-scheme-read-all
         wisp-scheme-read-chunk
         wisp-scheme-read-file
         wisp-scheme-read-string)

;; Copyright (C) Arne Babenhauserheide (2014--2015). All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.


;; Guile's throw, mapped onto Racket exceptions.
(define (throw key msg . irritants)
  (raise-user-error key "~a~a" msg
                    (apply string-append
                           (map (λ (i) (format " ~e" i)) irritants))))

;; Helper functions for the indent-and-symbols data structure: '((indent token token ...) ...)
(define (line-indent line)
         (car line))

(define (line-real-indent line)
         "Get the indentation without the comment-marker for unindented lines (-1 is treated as 0)."
         (let (( indent (line-indent line)))
             (if (= -1 indent)
               0
               indent)))

(define (line-code line)
         (let ((code (cdr line)))
             ; propagate source properties
             code))

; literal values I need
(define readcolon
       (string->symbol ":"))

(define wisp-uuid "e749c73d-c826-47e2-a798-c16c13cb89dd")
; define an intermediate dot replacement with UUID to avoid clashes.
(define repr-dot ; .
       (string->symbol (string-append "REPR-DOT-" wisp-uuid)))

; allow using reader additions as the first element on a line to prefix the list
(define repr-quote ; '
       (string->symbol (string-append "REPR-QUOTE-" wisp-uuid)))
(define repr-unquote ; ,
       (string->symbol (string-append "REPR-UNQUOTE-" wisp-uuid)))
(define repr-quasiquote ; `
       (string->symbol (string-append "REPR-QUASIQUOTE-" wisp-uuid)))
(define repr-unquote-splicing ; ,@
       (string->symbol (string-append "REPR-UNQUOTESPLICING-" wisp-uuid)))

(define repr-syntax ; #'
       (string->symbol (string-append "REPR-SYNTAX-" wisp-uuid)))
(define repr-unsyntax ; #,
       (string->symbol (string-append "REPR-UNSYNTAX-" wisp-uuid)))
(define repr-quasisyntax ; #`
       (string->symbol (string-append "REPR-QUASISYNTAX-" wisp-uuid)))
(define repr-unsyntax-splicing ; #,@
       (string->symbol (string-append "REPR-UNSYNTAXSPLICING-" wisp-uuid)))

; Racket's reader turns \: and |:| into a plain : before wisp sees
; them, so an escaped colon is marked here to keep it out of the
; inline-colon replacement and restored at the end.
(define repr-colon ; \: or |:|
       (string->symbol (string-append "REPR-COLON-" wisp-uuid)))

; literal vector as start of a line: ## a b -> #(a b). Guile extends its
; reader so ## yields #\#; here the lone token is caught like a prefix.
(define repr-hash ; ##
       (string->symbol (string-append "REPR-HASH-" wisp-uuid)))

; every char that can start or continue a list prefix; all ASCII, so
; byte offsets into the port equal char offsets while peeking them.
(define prefix-chars '(#\' #\, #\@ #\# #\` #\.))

; TODO: wrap the reader to return the repr of the syntax reader
; additions

(define (match-charlist-to-repr charlist)
         (let
           ((chlist (reverse charlist)))
           (cond
             ((equal? chlist (list #\.))
               repr-dot)
             ((equal? chlist (list #\'))
               repr-quote)
             ((equal? chlist (list #\,))
               repr-unquote)
             ((equal? chlist (list #\`))
               repr-quasiquote)
             ((equal? chlist (list #\, #\@ ))
               repr-unquote-splicing)
             ((equal? chlist (list #\# #\' ))
               repr-syntax)
             ((equal? chlist (list #\# #\, ))
               repr-unsyntax)
             ((equal? chlist (list #\# #\` ))
               repr-quasisyntax)
             ((equal? chlist (list #\# #\, #\@ ))
               repr-unsyntax-splicing)
             ((equal? chlist (list #\# #\# ))
               repr-hash)
             (else
               #f))))

(define (wisp-read port)
       "wrap read to catch list prefixes."
       ; Racket ports cannot unread-char, so peek ahead one char at a
       ; time and only consume the token when it is a lone prefix.
       ; Peeking stops at the first char that cannot continue a prefix,
       ; so no more input is awaited than the token itself needs (a
       ; whole-string peek would block a REPL or pipe on short tokens).
       (let ((prefix-maxlen 4))
         (let longpeek
           ((peeked '()))
           (let ((next-char (peek-char port (length peeked))))
             (cond
               ((or (eof-object? next-char)
                    (memv next-char '(#\space #\tab #\newline #\return)))
                 (let ((repr-symbol (match-charlist-to-repr peeked)))
                   (cond
                     (repr-symbol ; found a special symbol, consume and return it.
                       (read-string (length peeked) port)
                       repr-symbol)
                     (else
                       (wisp-read-datum port)))))
               ((and (< (length peeked) prefix-maxlen) (memv next-char prefix-chars))
                 (longpeek (cons next-char peeked)))
               (else
                 (wisp-read-datum port)))))))

(define (wisp-read-datum port)
       "let read do the work, but keep an escaped colon apart from the inline-colon operator."
       (cond
         ((skip-reader-directive! port)
           (void)) ; a directive yields no datum
         (else
           (let* ((escaped? (memv (peek-char port) '(#\\ #\|)))
                  (datum (read-with-curly-infix port)))
             (if (and escaped? (eq? datum readcolon))
                  repr-colon
                  datum)))))

;; Guile's #!curly-infix reader directive (SRFI-105). Curly infix is
;; always on here, so the directive is accepted and ignored. Racket
;; would otherwise treat #!curly-infix as a #lang line. A real #lang
;; line is skipped the same way, so a `#lang wisp` file can be read as
;; data (the module reader has already consumed it when a file is run).
(define (skip-reader-directive! port)
       "consume a #!curly-infix directive or a #lang line if one is next; return whether one was consumed"
       (define (next-is? directive)
         (let ((len (string-length directive)))
           (and (for/and ((ch (in-string directive))
                          (k (in-naturals)))
                  (eqv? ch (peek-char port k)))
                (let ((after (peek-char port len)))
                  (or (eof-object? after) (char-whitespace? after))))))
       (cond
         ((next-is? "#!curly-infix")
           (read-string (string-length "#!curly-infix") port)
           #t)
         ((next-is? "#lang")
           (read-line port)
           #t)
         (else #f)))

(define (skip-atmosphere! port)
       "consume whitespace and line comments so a directive check sees the next token"
       (let ((next-char (peek-char port)))
         (cond
           ((eof-object? next-char) (void))
           ((char-whitespace? next-char) (read-char port) (skip-atmosphere! port))
           ((eqv? #\; next-char) (read-line port) (skip-atmosphere! port))
           (else (void)))))

;; Guile enables SRFI-105 curly infix for wisp (read-enable 'curly-infix),
;; so {a + b} reads as (+ a b). Racket's reader keeps the paren shape on
;; syntax objects, which is enough for the plain infix forms. Neoteric
;; calls like f(x) inside braces are not supported.
(define (read-with-curly-infix port)
       "read one datum, turning {a op b op c} into (op a b c); #!curly-infix directives are skipped."
       (skip-atmosphere! port)
       (cond
         ((skip-reader-directive! port)
           (read-with-curly-infix port))
         (else
           (let ((stx (read-syntax (object-name port) port)))
             (if (eof-object? stx)
                  stx
                  (syntax->curly-infix-datum stx))))))

(define (syntax->curly-infix-datum stx)
       (let ((e (syntax-e stx)))
         (cond
           ((pair? e)
             (let ((items (syntax-pair->datum e)))
               (if (and (list? items) (eqv? #\{ (syntax-property stx 'paren-shape)))
                    (curly-infix->prefix items)
                    items)))
           ((vector? e)
             (list->vector (map syntax->curly-infix-datum (vector->list e))))
           ((box? e)
             (box (syntax->curly-infix-datum (unbox e))))
           (else
             (syntax->datum stx)))))

(define (syntax-pair->datum e)
       "walk the syntax-e of a pair, whose tail is a pair, a syntax object or a bare value"
       (cond
         ((pair? e)
           (cons (syntax->curly-infix-datum (car e)) (syntax-pair->datum (cdr e))))
         ((syntax? e)
           (syntax->curly-infix-datum e))
         (else
           e)))

(define (curly-infix->prefix items)
       "SRFI-105: {} -> (), {a} -> a, {a b} -> (a b), {a op b op c} -> (op a b c), anything else -> ($nfx$ ...)"
       (define (every-other lst) ; the operators: elements at odd positions
         (if (or (null? lst) (null? (cdr lst)))
              '()
              (cons (cadr lst) (every-other (cddr lst)))))
       (define (operands lst) ; elements at even positions
         (if (null? lst)
              '()
              (cons (car lst) (if (null? (cdr lst)) '() (operands (cddr lst))))))
       (cond
         ((null? items) '())
         ((null? (cdr items)) (car items))
         ((null? (cddr items)) items)
         ((and (odd? (length items))
               (let ((ops (every-other items)))
                 (andmap (λ (op) (equal? op (car ops))) ops)))
           (cons (cadr items) (operands items)))
         (else
           (cons '$nfx$ items))))



(define (line-continues? line)
         (equal? repr-dot (car (line-code line))))

(define (line-only-colon? line)
         (and
           (equal? readcolon (car (line-code line)))
           (null? (cdr (line-code line)))))

(define (line-empty-code? line)
         (null? (line-code line)))

(define (line-empty? line)
         (and
           ; if indent is -1, we stripped a comment, so the line was not really empty.
           (= 0 (line-indent line))
           (line-empty-code? line)))

(define (line-strip-continuation line   )
         (if (line-continues? line)
              (append
                (list
                  (line-indent line))
                (cdr (line-code line)))
              line))

(define (line-strip-indentation-marker line)
         "Strip the indentation markers from the beginning of the line"
         (cdr line))

(define (indent-level-reduction indentation-levels level select-fun)
         "Reduce the INDENTATION-LEVELS to the given LEVEL and return the value selected by SELECT-FUN"
         (let loop
           ((newlevels indentation-levels)
             (diff 0))
           (cond
             ((= level (car newlevels))
               (select-fun (list diff indentation-levels)))
             ((< level (car newlevels))
               (loop
                 (cdr newlevels)
                 (add1 diff)))
             (else
               (throw 'wisp-syntax-error
                 (format "Level ~a not found in the indentation-levels ~a." level indentation-levels))))))

(define (indent-level-difference indentation-levels level)
         "Find how many indentation levels need to be popped off to find the given level."
         (indent-level-reduction indentation-levels level
           (lambda (x ); get the count
                    (car x))))

(define (indent-reduce-to-level indentation-levels level)
         "Find how many indentation levels need to be popped off to find the given level."
         (indent-level-reduction indentation-levels level
           (lambda (x ); get the levels
                    (car (cdr x)))))

(define (chunk-ends-with-period currentsymbols next-char)
       "Check whether indent-and-symbols ends with a period, indicating the end of a chunk."
       (and (not (null? currentsymbols))
             (equal? #\newline next-char)
             (equal? repr-dot
                    (list-ref currentsymbols (- (length currentsymbols) 1)))))

(define (wisp-scheme-read-chunk-lines port)
         (let loop
           ((indent-and-symbols (list )); '((5 "(foobar)" "\"yobble\"")(3 "#t"))
             (inindent #t)
             (inunderscoreindent (equal? #\_ (peek-char port)))
             (incomment #f)
             (currentindent 0)
             (currentsymbols '())
             (emptylines 0))
           (cond
            ((>= emptylines 2 ); the chunk end has to be checked
                              ; before we look for new chars in the
                              ; port to make execution in the REPL
                              ; after two empty lines work
                              ; (otherwise it shows one more line).
             indent-and-symbols)
            (else
             (let ((next-char (peek-char port)))
               (cond
                 ((eof-object? next-char)
                   (append indent-and-symbols (list (append (list currentindent) currentsymbols))))
                 ((and inindent (zero? currentindent) (not incomment) (not (null? indent-and-symbols)) (not inunderscoreindent) (not (or (equal? #\space next-char) (equal? #\newline next-char) (equal? (string-ref ";" 0) next-char))))
                  (append indent-and-symbols )); top-level form ends chunk
                 ((chunk-ends-with-period currentsymbols next-char)
                   ; the line ends with a period. This is forbidden in
                   ; SRFI-119. Use it to end the line in the REPL without
                   ; showing continuation dots (...).
                   (append indent-and-symbols (list (append (list currentindent) (drop-right currentsymbols 1)))))
                 ((and inindent (equal? #\space next-char))
                   (read-char port ); remove char
                   (loop
                     indent-and-symbols
                     #t ; inindent
                     #f ; inunderscoreindent
                     #f ; incomment
                     (add1 currentindent)
                     currentsymbols
                     emptylines))
                 ((and inunderscoreindent (equal? #\_ next-char))
                   (read-char port ); remove char
                   (loop
                     indent-and-symbols
                     #t ; inindent
                     #t ; inunderscoreindent
                     #f ; incomment
                     (add1 currentindent)
                     currentsymbols
                     emptylines))
                 ; any char but whitespace *after* underscoreindent is
                 ; an error. This is stricter than the current wisp
                 ; syntax definition. TODO: Fix the definition. Better
                 ; start too strict. FIXME: breaks on lines with only
                 ; underscores which should empty lines.
                 ((and inunderscoreindent (and (not (equal? #\space next-char)) (not (equal? #\newline next-char))))
                   (throw 'wisp-syntax-error "initial underscores without following whitespace at beginning of the line after" (last indent-and-symbols)))
                 ((equal? #\newline next-char)
                   (read-char port ); remove the newline
                   ; The following two lines would break the REPL by requiring one char too many.
                   ; if : and (equal? #\newline next-char) : equal? #\return : peek-char port
                   ;      read-char port ; remove a full \n\r. Damn special cases...
                   (let* ; distinguish pure whitespace lines and lines
                        ; with comment by giving the former zero
                        ; indent. Lines with a comment at zero indent
                        ; get indent -1 for the same reason - meaning
                        ; not actually empty.
                     (
                       (indent
                         (cond
                           (incomment
                             (if (= 0 currentindent ); specialcase
                               -1
                               currentindent ))
                           ((not (null? currentsymbols )); pure whitespace
                             currentindent)
                           (else
                             0)))
                       (parsedline (append (list indent) currentsymbols))
                       (emptylines
                         (if (not (line-empty? parsedline))
                            0
                            (add1 emptylines))))
                     ; TODO: If the line is empty. Either do it here and do not add it, just
                     ; increment the empty line counter, or strip it later. Replace indent
                     ; -1 by indent 0 afterwards.
                     (loop
                       (append indent-and-symbols (list parsedline))
                       #t ; inindent
                       (if (<= 2 emptylines)
                         #f ; chunk ends here
                         (equal? #\_ (peek-char port ))); are we in underscore indent?
                       #f ; incomment
                       0
                       '()
                       emptylines)))
                 ((equal? #t incomment)
                   (read-char port ); remove one comment character
                   (loop
                     indent-and-symbols
                     #f ; inindent
                     #f ; inunderscoreindent
                     #t ; incomment
                     currentindent
                     currentsymbols
                     emptylines))
                 ((or (equal? #\space next-char) (equal? #\tab next-char) (equal? #\return next-char) ); remove whitespace when not in indent
                   (read-char port ); remove char
                   (loop
                     indent-and-symbols
                     #f ; inindent
                     #f ; inunderscoreindent
                     #f ; incomment
                     currentindent
                     currentsymbols
                     emptylines))
                          ; | cludge to appease the former wisp parser
                          ; | which had a problem with the literal comment
                          ; v char.
                 ((equal? (string-ref ";" 0) next-char)
                   (loop
                     indent-and-symbols
                     #f ; inindent
                     #f ; inunderscoreindent
                     #t ; incomment
                     currentindent
                     currentsymbols
                     emptylines))
                 (else ; use the reader
                   (loop
                     indent-and-symbols
                     #f ; inindent
                     #f ; inunderscoreindent
                     #f ; incomment
                     currentindent
                     ; this also takes care of the hashbang and leading comments.
                     (let ((datum (wisp-read port)))
                       (if (void? datum) ; a reader directive, which yields no datum
                            currentsymbols
                            (append currentsymbols (list datum))))
                     emptylines))))))))


(define (line-code-replace-inline-colons line)
         "Replace inline colons by opening parens which close at the end of the line"
         ; format #t "replace inline colons for line ~A\n" line
         (let loop
           ((processed '())
              (unprocessed line))
           (cond
             ((null? unprocessed)
               ; format #t "inline-colons processed line: ~A\n" processed
               processed)
             ; replace : . with nothing
             ((and (<= 2 (length unprocessed)) (equal? readcolon (car unprocessed)) (equal? repr-dot (car (cdr unprocessed))))
               (loop
                 (append processed
                   (loop '() (cdr (cdr unprocessed))))
                 '()))
             ((equal? readcolon (car unprocessed))
               (loop
                 ; FIXME: This should turn unprocessed into a list.
                 (append processed
                   (list (loop '() (cdr unprocessed))))
                 '()))
             (else
               (loop
                 (append processed
                   (list (car unprocessed)))
                 (cdr unprocessed))))))

(define (line-replace-inline-colons line)
         (cons
           (line-indent line)
           (line-code-replace-inline-colons (line-code line))))

(define (line-strip-lone-colon line)
         "A line consisting only of a colon is just a marked indentation level. We need to kill the colon before replacing inline colons."
         (if
           (equal?
             (line-code line)
             (list readcolon))
           (list (line-indent line))
           line))

(define (line-finalize line)
       "Process all wisp-specific information in a line and strip it"
       (let
         (
           (l
             (line-code-replace-inline-colons
               (line-strip-indentation-marker
                 (line-strip-lone-colon
                   (line-strip-continuation line))))))
         l))

; Guile tracked source properties on plain lists; Racket has no such
; side table, so source locations are attached per top-level form in
; wisp-read-syntax instead and these are no-ops.
(define (wisp-add-source-properties-from source target)
       target)

(define (wisp-propagate-source-properties code)
       code)

(define (wisp-scheme-indentation-to-parens lines)
         "Add parentheses to lines and remove the indentation markers"
         ; FIXME: Find new algorithm which mostly uses current-line
         ; and the indentation-levels for tracking. The try I have in
         ; here right now is wrong.
         (when
           (and
             (not (null? lines))
             (not (line-empty-code? (car lines)))
             (not (= 0 (line-real-indent (car lines ))))); -1 is a line with a comment
           (if (= 1 (line-real-indent (car lines)))
             ;; accept a single space as indentation of the first line (and ignore the indentation) to support meta commands
             (set! lines
                  (cons
                      (cons 0 (cdr (car lines)))
                      (cdr lines)))
             (throw 'wisp-syntax-error
               (format "The first symbol in a chunk must start at zero indentation. Two blank lines start a new chunk. Indentation and line: ~A"
                 (car lines)))))
         (let loop
           ((processed '())
             (unprocessed lines)
             (indentation-levels '(0)))
           (let*
             (
               (current-line
                 (if (<= 1 (length unprocessed))
                      (car unprocessed)
                      (list 0 ))); empty code
               (next-line
                 (if (<= 2 (length unprocessed))
                      (car (cdr unprocessed))
                      (list 0 ))); empty code
               (current-indentation
                      (car indentation-levels))
               (current-line-indentation (line-real-indent current-line)))
             ; format #t "processed: ~A\ncurrent-line: ~A\nnext-line: ~A\nunprocessed: ~A\nindentation-levels: ~A\ncurrent-indentation: ~A\n\n"
             ;     . processed current-line next-line unprocessed indentation-levels current-indentation
             (cond
                 ; the real end: this is reported to the outside world.
               ((and (null? unprocessed) (not (null? indentation-levels)) (null? (cdr indentation-levels)))
                 ; display "done\n"
                 ; reverse the processed lines, because I use cons.
                 processed)
               ; the recursion end-condition
               ((and (null? unprocessed))
                 ; display "last step\n"
                 ; this is the last step. Nothing more to do except
                 ; for rolling up the indentation levels.  return the
                 ; new processed and unprocessed lists: this is a
                 ; side-recursion
                 (values processed unprocessed))
               ((null? indentation-levels)
                 ; display "indentation-levels null\n"
                 (throw 'wisp-programming-error "The indentation-levels are null but the current-line is null: Something killed the indentation-levels."))
               (else ; now we come to the line-comparisons and indentation-counting.
                   (cond
                     ((line-empty-code? current-line)
                       ; display "current-line empty\n"
                       ; We cannot process indentation without
                       ; code. Just switch to the next line. This should
                       ; only happen at the start of the recursion.
                       ; TODO: Somehow preserve the line-numbers.
                       (loop
                         processed
                         (cdr unprocessed)
                         indentation-levels))
                     ((and (line-empty-code? next-line) (<= 2 (length unprocessed )))
                       ; display "next-line empty\n"
                       ; TODO: Somehow preserve the line-numbers.
                       ; take out the next-line from unprocessed.
                       (loop
                         processed
                         (cons current-line
                           (cdr (cdr unprocessed)))
                         indentation-levels))
                     ((> current-indentation current-line-indentation)
                       ; display "current-indent > next-line\n"
                       ; this just steps back one level via the side-recursion.
                       (let ((previous-indentation (car (cdr indentation-levels))))
                         (if (<= current-line-indentation previous-indentation)
                            (values processed unprocessed)
                            (begin ;; not yet used level! TODO: maybe throw an error here instead of a warning.
                                (let ((linenumber (- (length lines) (length unprocessed))))
                                    (eprintf ";;; WARNING: used lower but undefined indentation level (line ~a of the current chunk: ~s). This makes refactoring much more error-prone, therefore it might become an error in a later version of Wisp.\n" linenumber (cdr current-line)))
                                (loop
                                  processed
                                  unprocessed
                                  (cons ; recursion via the indentation-levels
                                    current-line-indentation
                                    (cdr indentation-levels)))))))
                     ((= current-indentation current-line-indentation)
                       ; display "current-indent = next-line\n"
                       (let
                         ((line (line-finalize current-line))
                           (next-line-indentation (line-real-indent next-line)))
                         (cond
                           ((>= current-line-indentation next-line-indentation)
                             ; simple recursiive step to the next line
                             ; display "current-line-indent >= next-line-indent\n"
                             (loop
                               (append processed
                                 (if (line-continues? current-line)
                                      line
                                      (wisp-add-source-properties-from line (list line))))
                               (cdr unprocessed ); recursion here
                               indentation-levels))
                           ((< current-line-indentation next-line-indentation)
                             ; display "current-line-indent < next-line-indent\n"
                             ; format #t "line: ~A\n" line
                             ; side-recursion via a sublist
                             (let-values
                               (
                                 ((sub-processed sub-unprocessed)
                                   (loop
                                     line
                                     (cdr unprocessed ); recursion here
                                     indentation-levels)))
                               ; format #t "side-recursion:\n  sub-processed: ~A\n  processed: ~A\n\n" sub-processed processed
                               (loop
                                 (append processed (list sub-processed))
                                 sub-unprocessed ; simply use the recursion from the sub-recursion
                                 indentation-levels))))))
                     ((< current-indentation current-line-indentation)
                       ; display "current-indent < next-line\n"
                       (loop
                         processed
                         unprocessed
                         (cons ; recursion via the indentation-levels
                           current-line-indentation
                           indentation-levels)))
                     (else
                       (throw 'wisp-not-implemented
                             (format "Need to implement further line comparison: current: ~A, next: ~A, processed: ~A."
                               current-line next-line processed)))))))))


(define (wisp-scheme-replace-inline-colons lines)
         "Replace inline colons by opening parens which close at the end of the line"
         (let loop
           ((processed '())
             (unprocessed lines))
           (if (null? unprocessed)
                processed
                (loop
                  (append processed (list (line-replace-inline-colons (car unprocessed))))
                  (cdr unprocessed)))))


(define (wisp-scheme-strip-indentation-markers lines)
         "Strip the indentation markers from the beginning of the lines"
         (let loop
           ((processed '())
             (unprocessed lines))
           (if (null? unprocessed)
                processed
                (loop
                  (append processed (cdr (car unprocessed)))
                  (cdr unprocessed)))))

(define (wisp-unescape-underscore-and-colon code)
         "restore an escaped colon to : (Racket's reader already unescapes \\_ to _ on its own)"
         (match code
             ((list a ...)
               (map wisp-unescape-underscore-and-colon a))
             ((== repr-colon)
               readcolon)
             (a
               a)))


(define (wisp-replace-empty-eof code)
         "replace ((#<eof>)) by ()"
         ; FIXME: Actually this is a hack which fixes a bug when the
         ; parser hits files with only hashbang and comments.
         (if (and (not (null? code)) (pair? (car code)) (eof-object? (car (car code))) (null? (cdr code)) (null? (cdr (car code))))
              (list)
              code))


(define (wisp-replace-paren-quotation-repr code)
         "Replace lists starting with a quotation symbol by
         quoted lists."
         (match code
             ((list 'REPR-QUOTE-e749c73d-c826-47e2-a798-c16c13cb89dd a ...)
                (list 'quote (map wisp-replace-paren-quotation-repr a)))
             ((list a ... 'REPR-QUOTE-e749c73d-c826-47e2-a798-c16c13cb89dd b ); lone prefix before the last datum, e.g. the quoted empty list
                (append
                        (map wisp-replace-paren-quotation-repr a)
                        (list (list 'quote (wisp-replace-paren-quotation-repr b)))))
             ((list 'REPR-QUASIQUOTE-e749c73d-c826-47e2-a798-c16c13cb89dd 'REPR-UNQUOTE-e749c73d-c826-47e2-a798-c16c13cb89dd a ...)
                (list 'quasiquote (list 'unquote (map wisp-replace-paren-quotation-repr a))))
             ((list 'REPR-UNQUOTE-e749c73d-c826-47e2-a798-c16c13cb89dd a ...)
                (list 'unquote (map wisp-replace-paren-quotation-repr a)))
             ((list a ... 'REPR-UNQUOTE-e749c73d-c826-47e2-a798-c16c13cb89dd b )
                (append
                        (map wisp-replace-paren-quotation-repr a)
                        (list (list 'unquote (wisp-replace-paren-quotation-repr b)))))
             ((list 'REPR-QUASIQUOTE-e749c73d-c826-47e2-a798-c16c13cb89dd a ...)
                (list 'quasiquote (map wisp-replace-paren-quotation-repr a)))
             ((list a ... 'REPR-QUASIQUOTE-e749c73d-c826-47e2-a798-c16c13cb89dd b ); lone prefix before the last datum
                (append
                        (map wisp-replace-paren-quotation-repr a)
                        (list (list 'quasiquote (wisp-replace-paren-quotation-repr b)))))
             ((list 'REPR-UNQUOTESPLICING-e749c73d-c826-47e2-a798-c16c13cb89dd a ...)
                (list 'unquote-splicing (map wisp-replace-paren-quotation-repr a)))
             ((list 'REPR-SYNTAX-e749c73d-c826-47e2-a798-c16c13cb89dd a ...)
                (list 'syntax (map wisp-replace-paren-quotation-repr a)))
             ((list 'REPR-UNSYNTAX-e749c73d-c826-47e2-a798-c16c13cb89dd a ...)
                (list 'unsyntax (map wisp-replace-paren-quotation-repr a)))
             ((list 'REPR-QUASISYNTAX-e749c73d-c826-47e2-a798-c16c13cb89dd a ...)
                (list 'quasisyntax (map wisp-replace-paren-quotation-repr a)))
             ((list 'REPR-UNSYNTAXSPLICING-e749c73d-c826-47e2-a798-c16c13cb89dd a ...)
                (list 'unsyntax-splicing (map wisp-replace-paren-quotation-repr a)))
             ;; literal array as start of a line: ## a b -> #(a b)
             ((list (== repr-hash) a ...)
               (list->vector (map wisp-replace-paren-quotation-repr a)))
             ((list a ...)
               (map wisp-replace-paren-quotation-repr a))
             (a
               a)))

(define (wisp-make-improper code)
         "Turn (a #{.}# b) into the correct (a . b).

read called on a single dot creates a variable named #{.}# (|.|
in r7rs). Due to parsing the indentation before the list
structure is known, the reader cannot create improper lists
when it reads a dot. So we have to take another pass over the
code to recreate the improper lists.

Match is awesome!"
         (let
           (
             (improper
               (match code
                  ((list a ... b 'REPR-DOT-e749c73d-c826-47e2-a798-c16c13cb89dd c)
                    (append (map wisp-make-improper a)
                      (cons (wisp-make-improper b) (wisp-make-improper c))))
                  ((list a ...)
                    (map wisp-make-improper a))
                  (a
                    a))))
           ; The upstream Guile version carried a disabled (if #t ...)
           ; validity check here; it relied on Guile-only last-pair and
           ; never ran, so it was dropped in the Racket port.
           improper))

(define (wisp-scheme-read-chunk port)
         "Read and parse one chunk of wisp-code"
         (let (( lines (wisp-scheme-read-chunk-lines port)))
              (wisp-make-improper
                (wisp-replace-empty-eof
                  (wisp-unescape-underscore-and-colon
                    (wisp-replace-paren-quotation-repr
                      (wisp-propagate-source-properties
                        (wisp-scheme-indentation-to-parens lines))))))))

(define (wisp-scheme-read-all port)
         "Read all chunks from the given port"
         (let loop
           ((tokens '()))
           (cond
             ((eof-object? (peek-char port))
               tokens)
             (else
               (loop
                 (append tokens (wisp-scheme-read-chunk port)))))))

(define (wisp-scheme-read-file path)
         (call-with-input-file path wisp-scheme-read-all))

(define (wisp-scheme-read-file-chunk path)
         (call-with-input-file path wisp-scheme-read-chunk))

(define (wisp-scheme-read-string str)
         (wisp-scheme-read-all (open-input-string str)))

(define (wisp-scheme-read-string-chunk str)
         (wisp-scheme-read-chunk (open-input-string str)))

(define (wisp-read-syntax src port)
         "Read all chunks as syntax objects, each top-level form carrying the location of the chunk it came from."
         (port-count-lines! port)
         (let loop
           ((forms '()))
           (cond
             ((eof-object? (peek-char port))
               forms)
             (else
               (let*-values
                 (((line col pos) (port-next-location port))
                  ((chunk) (wisp-scheme-read-chunk port))
                  ((_l _c end) (port-next-location port))
                  ((srcloc) (vector src line col pos (and pos end (- end pos)))))
                 (loop
                   (append forms
                     (for/list ((form (in-list chunk)))
                       (datum->syntax #f form srcloc)))))))))


;;;; Test special syntax
; ;; quote the list
; write
;   wisp-scheme-read-string  "moo
;   foo
;     ' bar
; baz waz"
; newline
; ;; quote the symbol - in wisp, whitespace after quote is not allowed!
; write
;   wisp-scheme-read-string  "moo
;   foo
;     'bar
; baz waz"
; newline
; ; ;; quote the list with colon
; write
;   wisp-scheme-read-string  "moo : ' foo
;   foo
;     ' bar bah
; baz waz"
; newline
; ; ;; syntax the list
; write
;   wisp-scheme-read-string  "moo : #' foo
;   foo
;     #' bar bah
; baz waz"
; newline
;
;;;; Test improper lists
;;;; Good cases
; write
;   wisp-scheme-read-string  "foo . bar"
; newline
; write
;   wisp-scheme-read-string  "foo .
;   . bar"
; newline
; write
;   wisp-scheme-read-string  "foo
;   . . bar"
; newline
; write
;   wisp-scheme-read-string  "moo
;   foo
;     . . bar
; baz waz"
; newline
;;;; Syntax Error cases
; write
;   wisp-scheme-read-string  "foo
;   . . ."
; newline
; write
;   wisp-scheme-read-string  "moo : . . bar"
; write
;   wisp-scheme-read-string  "foo .
;   . . bar"
; newline
; write
;   wisp-scheme-read-string  "moo
;   foo
;     . . bar baz
; baz waz"
; newline
;;;; stranger stuff
; write
;   wisp-scheme-read-string  "foo ; bar\n  ; nop \n\n; nup\n; nup \n  \n\n\nfoo : moo \"\n\" \n___ . goo . hoo"
; newline
; display
;   wisp-scheme-read-string  "  foo ; bar\n  ; nop \n\n; nup\n; nup \n  \n\n\nfoo : moo"
; newline
; write : wisp-scheme-read-file-chunk "wisp-scheme.w"
; newline
; call-with-output-file "wisp-guile.scm"
;   lambda : port
;     map
;        lambda : chunk
;                 write chunk port
;        wisp-scheme-read-file "wisp-guile.w"
; run all chunks in wisp-guile.w as parsed by wisp-scheme.w. Give wisp-guile.w to parse as argument.
; map primitive-eval : wisp-scheme-read-file "wisp-guile.w" ; actually runs wisp-guile.w with the arguments supplied to this script.
; uncomment the previous line, then run the next line in the shell. If 1 and 2 are equal, this parser works!
; guile wisp.scm wisp-scheme.w > wisp-scheme.scm; guile wisp-scheme.scm wisp-guile.w > 1; guile wisp.scm wisp-guile.w > 2; diff 1 2
