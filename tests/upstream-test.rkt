#lang racket

;; Port of upstream wisp's tests/runtests-scheme-reader.sh + testrunner.w:
;; every tests/upstream/NAME.w must parse to the same data as reading
;; tests/upstream/NAME.scm with the plain reader.
;;
;; The .scm files are read with curly infix enabled, as Guile does once
;; (language wisp) has run read-enable 'curly-infix.

(require rackunit
         racket/runtime-path
         "../parse.rkt")

(define-runtime-path upstream "upstream")

(define (read-scheme-file path)
  (call-with-input-file path
    (λ (in)
      (let loop ((res '()))
        (let ((next (read-with-curly-infix in)))
          (if (eof-object? next)
              (reverse res)
              (loop (cons next res))))))))

(define wisp-files
  (sort (filter (λ (p) (path-has-extension? p #".w"))
                (directory-list upstream))
        path<?))

(for ((w wisp-files))
  (define scm (path-replace-extension w #".scm"))
  (test-case (path->string w)
    (check-equal? (wisp-scheme-read-file (build-path upstream w))
                  (read-scheme-file (build-path upstream scm)))))
