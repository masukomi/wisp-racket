#lang s-exp syntax/module-reader
;; `#lang wisp` reads a module body written in wisp and expands it as `racket`.
racket
#:read (λ (in) (wisp-scheme-read-all in))
#:read-syntax wisp-read-syntax
#:whole-body-readers? #t
(require "../parse.rkt")
