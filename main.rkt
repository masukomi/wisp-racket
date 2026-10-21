#lang racket

;; Implement wisp in Racket
;;
;; -Author: Arne Babenhauserheide

; adapted from wisp-in-guile and algol60.rkt
;
; The `#lang wisp` reader lives in lang/reader.rkt; this module offers
; the parser as a library plus `literal-wisp` for embedding wisp code
; in ordinary Racket modules.

(require "parse.rkt"
         (for-syntax "parse.rkt"))

(provide literal-wisp
         wisp-scheme-read-all
         wisp-scheme-read-file
         wisp-scheme-read-string)


;; (literal-wisp "display 1\n" "newline\n") expands to the parsed forms.
(define-syntax (literal-wisp stx)
  (syntax-case stx ()
    [(_ strs ...)
     (andmap (λ (x) (string? (syntax-e x)))
             (syntax->list (syntax (strs ...))))

     (datum->syntax
      stx
      (cons 'begin
            (wisp-scheme-read-string
             (apply
              string-append
              (map syntax-e (syntax->list #'(strs ...))))))
      stx)]))
