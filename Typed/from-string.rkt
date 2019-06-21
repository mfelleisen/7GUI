#lang typed/racket

(provide string->er)

;; Racket should have an `exact-rational?` predicate so that it matches Typed Racket's Exact-Rational.

(require 7GUI/should-be-racket)

(: string->er (String -> (U Exact-Rational False)))
(define (string->er s)
  (define r (parameterize ([read-decimal-as-inexact #f]) (string->number s)))
  (and (rational? r) (exact? r) (ann r Exact-Rational)))

(~r (cast (string->er "1.34") Exact-Rational) #:precision 4)
(string->er "abc")

