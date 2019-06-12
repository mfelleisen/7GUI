#lang typed/racket

(provide string->er)

;; Racket should have an `exact-rational?` predicate so that it matches Typed Racket's Exact-Rational.

(: string->er (String -> (U Exact-Rational False)))
(define (string->er s)
  (define r (parameterize ([read-decimal-as-inexact #f]) (string->number s)))
  (and r (rational? r) (exact? r) (ann r Exact-Rational)))

(~r (cast (string->er "1.34") Exact-Rational) #:precision 4)
(string->er "abc")

