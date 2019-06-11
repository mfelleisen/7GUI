#lang typed/racket

(provide string->er)

(: string->er (String -> (U Exact-Rational False)))
(define (string->er s)
  (define r (string->number s))
  ;; what is the predicate for Exact-Rational ???
  (and r (if (real? r) (cast r Exact-Rational) #f)))

