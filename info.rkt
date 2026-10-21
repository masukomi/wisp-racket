#lang info
(define collection "wisp")
(define deps '("base" "syntax-color-lib"))
(define build-deps '("rackunit-lib"))
; tests/upstream holds .w/.scm fixture pairs, not Racket modules
(define compile-omit-paths '("tests/upstream"))
(define test-omit-paths '("tests/upstream"))
(define pkg-desc "Wisp (SRFI-119): indentation-sensitive syntax for Racket, usable as #lang wisp")
(define version "0.1")
(define license 'MIT)
(define pkg-authors '("Arne Babenhauserheide"))
