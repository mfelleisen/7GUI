#lang typed/racket

(provide string->er)

;; Racket should have an `exact-rational?` predicate so that it matches Typed Racket's Exact-Rational.

(: string->er (String -> (U Exact-Rational False)))
;; convert string to exact rational if needed
(define (string->er s)
  (define r (parameterize ([read-decimal-as-inexact #f]) (string->number s)))
  (and (rational? r) (exact? r) (ann r Exact-Rational)))