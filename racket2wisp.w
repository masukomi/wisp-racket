#lang wisp
;; racket2wisp: convert Racket source into wisp (SRFI-119) syntax.
;;
;; Two front ends build the same intermediate tree (IR):
;;   lex->ir          tokenizes with syntax-color/racket-lexer and keeps comments
;;   read-syntax->ir  uses the Racket reader and drops comments
;; One emitter, ir->wisp, turns the tree into indented wisp text.
;;
;; This file is itself written in wisp.

require racket/string racket/list racket/port racket/match syntax-color/racket-lexer "parse.rkt"

provide
  struct-out atom
  struct-out lst
  struct-out comment
  struct-out blank
  struct-out wisp-file
  . read-syntax->ir lex->ir datum->node ir->wisp racket->wisp file->wisp
  . current-indent-width current-line-width

;; ---------------------------------------------------------------------
;; IR

;; text is the source text (or ~s of the datum); datum is the read value.
struct atom (text datum) #:transparent
;; opener is "(" "[" "{" "#(" "#hash(" ...; tail is a node for improper lists or #f;
;; prefix is "" or a reader prefix such as "'" "`" "," ",@" "#'" (possibly chained);
;; multi is #t when the list spanned several source lines, #f when not, or
;; 'auto when the front end does not know (the emitter then guesses).
struct lst (opener items tail prefix multi) #:transparent
;; text is verbatim: "; ..." "#| ... |#" or "#;datum". trailing? = same line as the previous node.
struct comment (text trailing?) #:transparent
;; count blank lines between two siblings.
struct blank (count) #:transparent
;; lang is the #lang name as a string, or #f for a plain data file.
struct wisp-file (lang nodes) #:transparent

define current-indent-width : make-parameter 2
define current-line-width : make-parameter 80

;; longest first, so split-prefix matches ",@" before ","
define prefixes : list "#,@" "#," "#`" "#'" ",@" "," "`" "'"

define prefix-names
  hash "'" 'quote "`" 'quasiquote "," 'unquote ",@" 'unquote-splicing
    . "#'" 'syntax "#`" 'quasisyntax "#," 'unsyntax "#,@" 'unsyntax-splicing

define prefix-symbols
  for/hash : ((p name) (in-hash prefix-names))
    values name p

define : split-prefix s
  if : string=? s ""
    . '()
    let : : p (findf (λ (p) (string-prefix? s p)) prefixes)
      cons p : split-prefix : substring s : string-length p

;; ---------------------------------------------------------------------
;; emitter helpers

define : pad n
  make-string n #\space

define : closer opener
  case : string-ref opener : sub1 : string-length opener
    (#\[) "]"
    (#\{) "}"
    else ")"

;; a lone ":" is wisp's inline colon
define : escape-atom text
  if (string=? text ":") "\\:" text

;; and a leading "_" at the start of a line is indentation
define : escape-head text
  if (string-prefix? text "_") (string-append "\\" text) text

define : comment-or-blank? node
  or (comment? node) (blank? node)

;; a list that wisp indentation can express: parens or brackets, proper, non-empty
define : block-list? node
  and
    lst? node
    member (lst-opener node) '("(" "[")
    not : lst-tail node
    pair? : lst-items node

define : plain-block? node
  and (block-list? node) : string=? "" : lst-prefix node

define : atom-headed? node
  atom? : car : lst-items node

;; did the list span several lines? With no layout information, guess from the shape.
define : multi? node
  define m : lst-multi node
  if : eq? m 'auto
    >= (count plain-block? (lst-items node)) 2
    . m

;; can the node be written as ordinary s-expression text on one line?
define : inline-ok? node
  cond
    : comment-or-blank? node
      . #f
    : lst? node
      and
        not : multi? node
        andmap inline-ok? : lst-items node
        or (not (lst-tail node)) : inline-ok? : lst-tail node
    else #t

;; the node rendered as ordinary s-expression text; comments inside are dropped
define : inline node
  cond
    : atom? node
      escape-atom : atom-text node
    : lst? node
      string-append
        lst-prefix node
        lst-opener node
        string-join (map inline (filter-not comment-or-blank? (lst-items node))) " "
        if : lst-tail node
          string-append " . " : inline : lst-tail node
          . ""
        closer : lst-opener node
    else ""

define : fits? str col
  <= (+ col (string-length str)) : current-line-width

;; usable after an inline ":" or as the items of a line
define : colon-able? node
  and (plain-block? node) : inline-ok? node

;; items joined for one line, escaping the first token
define : join-items items
  define strs : map inline items
  string-join (cons (escape-head (car strs)) (cdr strs)) " "

;; the list as one wisp line (without indentation): items joined, "' a b" for a prefix
define : line-form node
  if : string=? "" : lst-prefix node
    join-items : lst-items node
    string-append (lst-prefix node) " " : string-join (map inline (lst-items node)) " "

;; can node be appended to a line that is currently `line`?
define : stays? node line
  and
    inline-ok? node
    fits? (string-append " " (inline node)) : string-length line

define : trailing-comment? node
  and (comment? node) : comment-trailing? node

;; ---------------------------------------------------------------------
;; emitter

;; a node in child position: lists become lines, everything else needs ". "
define : emit-child node indent
  cond
    : atom? node
      list : string-append (pad indent) ". " : escape-atom : atom-text node
    : comment? node
      list : string-append (pad indent) : comment-text node
    : blank? node
      make-list (if (zero? indent) (min 2 (blank-count node)) 1) ""
    : lst? node
      emit-list node indent

;; siblings in child position; trailing comments attach to the previous line
define : emit-children items indent
  let loop : (items items) (acc '())
    cond
      : null? items
        reverse acc
      : and (trailing-comment? (car items)) (pair? acc)
        loop (cdr items) : cons (string-append (car acc) " " (comment-text (car items))) : cdr acc
      else
        loop (cdr items) : append (reverse (emit-child (car items) indent)) acc

define : emit-list node indent
  cond
    : not : block-list? node
      list : string-append (pad indent) ". " : inline node
    : and (inline-ok? node) : fits? (inline node) indent
      emit-single node indent
    : not : string=? "" : lst-prefix node
      emit-list (expand-prefix node) indent
    : atom-headed? node
      emit-atom-head node indent
    else
      emit-list-head node indent

;; a list that was on one source line and fits: one line, with wisp's idioms
define : emit-single node indent
  define items : lst-items node
  define child-indent : + indent : current-indent-width
  cond
    : not : string=? "" : lst-prefix node
      list : string-append (pad indent) : line-form node
    ;; ((x 1) (y 2)) becomes ": x 1" over "y 2", the let-bindings idiom
    : andmap colon-able? items
      cons
        string-append (pad indent) ": " : line-form : car items
        map (λ (n) (string-append (pad child-indent) (line-form n))) : cdr items
    ;; (f a (g b)) becomes "f a : g b"
    : and (pair? (cdr items)) (colon-able? (last items)) (atom-headed? (last items))
      list : string-append (pad indent) (join-items (drop-right items 1)) " : " : line-form : last items
    else
      list : string-append (pad indent) : join-items items

;; '(...) that does not fit on a line becomes (quote ...) so the head rules apply
define : expand-prefix node
  let*
    : ps : split-prefix : lst-prefix node
      inner : struct-copy lst node (prefix "")
    for/fold : (n inner)
      : p : reverse ps
      let : : name : hash-ref prefix-names p
        lst "(" (list (atom (symbol->string name) name) n) #f "" #t

;; head is an atom: atoms stay on the line, the first plain list may follow a colon,
;; everything from the first list on becomes a child line
define : emit-atom-head node indent
  define items : lst-items node
  define child-indent : + indent : current-indent-width
  let loop
    : line : string-append (pad indent) : escape-head : escape-atom : atom-text : car items
      rest : cdr items
    cond
      : null? rest
        list line
      : trailing-comment? : car rest
        cons (string-append line " " (comment-text (car rest))) : emit-children (cdr rest) child-indent
      : plain-block? : car rest
        if : and (colon-able? (car rest)) (atom-headed? (car rest)) (fits? (string-append " : " (line-form (car rest))) (string-length line))
          cons (string-append line " : " (line-form (car rest))) : emit-children (cdr rest) child-indent
          cons line : emit-children rest child-indent
      : stays? (car rest) line
        loop (string-append line " " (inline (car rest))) : cdr rest
      else
        cons line : emit-children rest child-indent

;; head is a list (or a comment): ": head-items" opens the list, the rest are children
define : emit-list-head node indent
  define items : lst-items node
  define head : car items
  define child-indent : + indent : current-indent-width
  cond
    : and (colon-able? head) : fits? (string-append ": " (line-form head)) indent
      cons (string-append (pad indent) ": " (line-form head)) : emit-children (cdr items) child-indent
    else
      cons (string-append (pad indent) ":") : emit-children items child-indent

define : ir->wisp file
  define lang : wisp-file-lang file
  when : and lang : not : string=? lang "racket"
    eprintf "racket2wisp: warning: #lang ~a becomes #lang wisp, which expands as racket\n" lang
  define nodes : dropf-right (dropf (wisp-file-nodes file) blank?) blank?
  string-append
    if lang "#lang wisp\n\n" ""
    string-join (emit-children nodes 0) "\n"
    . "\n"

;; ---------------------------------------------------------------------
;; front end A: Racket reader (no comments)

define : source-lang src
  define m : regexp-match #px"^#lang\\s+(\\S+)" src
  and m : cadr m

define : split-improper d
  let loop : (d d) (acc '())
    cond
      : pair? d
        loop (cdr d) : cons (car d) acc
      : null? d
        values (reverse acc) #f
      else
        values (reverse acc) : list d

define : prefix-node p inner
  cond
    : lst? inner
      struct-copy lst inner (prefix (string-append p (lst-prefix inner)))
    else
      atom (string-append p (atom-text inner)) : list (hash-ref prefix-names p) (atom-datum inner)

define : pair->node d
  define-values (items tail) : split-improper d
  cond
    : and (= 2 (length items)) (not tail) (hash-ref prefix-symbols (car items) #f)
      prefix-node (hash-ref prefix-symbols (car items)) : datum->node : cadr items
    else
      lst "(" (map datum->node items) (and tail (datum->node (car tail))) "" 'auto

define : datum->node d
  cond
    : pair? d
      pair->node d
    : null? d
      lst "(" '() #f "" #f
    : vector? d
      lst "#(" (map datum->node (vector->list d)) #f "" 'auto
    else
      atom (format "~s" d) d

define : module-body-forms src
  define in : open-input-string src
  port-count-lines! in
  define stx
    parameterize : (read-accept-reader #t) (read-accept-lang #t)
      read-syntax 'racket2wisp in
  match : syntax->datum stx
    : list 'module _ _ (list '#%module-begin forms ...)
      . forms
    \_
      raise-user-error 'racket2wisp "expected a module form from the #lang reader"

define : read-all-data src curly?
  define in : open-input-string src
  let loop : (acc '())
    define next : if curly? (read-with-curly-infix in) (read in)
    if : eof-object? next
      reverse acc
      loop : cons next acc

;; curly-infix 'auto means: on for plain data files, off for #lang modules
define : read-syntax->ir in #:curly-infix (curly 'auto)
  define src : port->string in
  define lang : source-lang src
  define forms
    if lang
      module-body-forms src
      read-all-data src : if (eq? curly 'auto) #t curly
  wisp-file lang : add-between (map datum->node forms) : blank 1

;; ---------------------------------------------------------------------
;; front end B: lexer (keeps comments)

struct frame (opener prefix sexp-start trailing? start (items #:mutable) (dot? #:mutable) (tail #:mutable))

;; "#;datum" cannot end a wisp line (the reader would skip the next line's first
;; datum too), so datum comments become line comments
define : sexp-comment->line-comment text
  string-join
    map (λ (l) (string-append "; " l)) : string-split text "\n" #:trim? #f
    . "\n"

define : lex-error msg tok pos
  raise-user-error 'racket2wisp "~a at offset ~a: ~s" msg pos tok

define : count-newlines s
  for/sum : (c s)
    if (char=? c #\newline) 1 0

define : read-datum text
  with-handlers : (exn:fail? (λ (e) text))
    read : open-input-string text

define : uncurl node
  cond
    : lst? node
      lst
        if (string=? "{" (lst-opener node)) "(" : lst-opener node
        map uncurl : lst-items node
        and (lst-tail node) : uncurl : lst-tail node
        lst-prefix node
        lst-multi node
    else node

define : lex->ir in #:curly-infix (curly 'auto)
  define src : port->string in
  define sin : open-input-string src
  port-count-lines! sin
  define top : frame "" "" #f #f 1 '() #f #f
  define stack : list top
  define lang #f
  define prefix ""
  define sexp-start #f
  define newline? #t
  define : slice s e
    substring src (- s 1) (- e 1)
  define : cur
    car stack
  define : after-something?
    pair? : frame-items : cur
  define : add! node
    if : frame-dot? : cur
      set-frame-tail! (cur) node
      set-frame-items! (cur) : cons node : frame-items : cur
    unless : blank? node
      set! newline? #f
  define : add-datum! node e
    cond
      sexp-start
        add! : comment (sexp-comment->line-comment (slice sexp-start e)) : not newline?
        set! sexp-start #f
      else
        add! node
  define : open! tok s
    set! stack : cons (frame tok prefix sexp-start (not newline?) s '() #f #f) stack
    set! prefix ""
    set! sexp-start #f
  define : close! tok e
    when : null? : cdr stack
      lex-error "unbalanced closing paren" tok e
    define f : car stack
    set! stack : cdr stack
    if : frame-sexp-start f
      add! : comment (sexp-comment->line-comment (slice (frame-sexp-start f) e)) : frame-trailing? f
      add!
        lst (frame-opener f) (reverse (frame-items f)) (frame-tail f) (frame-prefix f)
          string-contains? (slice (frame-start f) e) "\n"
  define : datum! tok s e
    cond
      : member tok prefixes
        set! prefix : string-append prefix tok
      else
        add-datum! (atom (string-append prefix tok) (read-datum (string-append prefix tok))) e
        set! prefix ""
  define : other! tok s e
    cond
      : member tok prefixes
        set! prefix : string-append prefix tok
      : string=? tok "."
        set-frame-dot?! (cur) #t
      : string-prefix? tok "#lang "
        set! lang : string-trim : substring tok 6
      : string-prefix? tok "#!"
        void
      else
        lex-error "unsupported token" tok s
  define : whitespace! tok
    define n : count-newlines tok
    when : >= n 1
      set! newline? #t
    when : and (>= n 2) : after-something?
      add! : blank : sub1 n
  let loop ()
    define-values (text type paren s e) : racket-lexer sin
    unless : eof-object? text
      define tok : slice s e
      case type
        (white-space)
          whitespace! tok
        (comment)
          add! : comment tok : and (not newline?) : after-something?
        (sexp-comment)
          set! sexp-start s
        (parenthesis)
          if : member (string-ref tok (sub1 (string-length tok))) '(#\( #\[ #\{)
            open! tok s
            close! tok e
        (constant symbol string hash-colon-keyword)
          datum! tok s e
        (other)
          other! tok s e
        else
          lex-error (format "unsupported ~a token" type) tok s
      loop
  unless : null? : cdr stack
    lex-error "unclosed paren" (frame-opener (car stack)) (string-length src)
  define curly? : if (eq? curly 'auto) (not lang) curly
  define nodes : reverse : frame-items top
  wisp-file lang : if curly? nodes : map uncurl nodes

;; ---------------------------------------------------------------------
;; entry points

define : racket->wisp src #:comments? (comments? #t) #:curly-infix (curly 'auto)
  define in : open-input-string src
  ir->wisp
    if comments?
      lex->ir in #:curly-infix curly
      read-syntax->ir in #:curly-infix curly

define : file->wisp path #:comments? (comments? #t) #:curly-infix (curly 'auto)
  racket->wisp (file->string path) #:comments? comments? #:curly-infix curly

module+ main
  require racket/cmdline
  define out #f
  define comments? #t
  define file
    command-line
      . #:program "racket2wisp"
      . #:once-each
      ("-o" "--output") path "Write to PATH instead of stdout" (set! out path)
      ("--no-comments") "Drop comments: use the Racket reader instead of the lexer" (set! comments? #f)
      ("--indent") n "Indentation width (default 2)" (current-indent-width (string->number n))
      ("--width") n "Target line width (default 80)" (current-line-width (string->number n))
      . #:args (file) file
  define result : file->wisp file #:comments? comments?
  void
    if out
      call-with-output-file out (λ (o) (write-string result o)) #:exists 'truncate
      write-string result
