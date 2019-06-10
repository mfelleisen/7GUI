#! /usr/bin/env gracket
#lang typed/racket/gui

;; We need something like this in Typed Racket. 
(: string->er (String -> (U Exact-Rational False)))
(define (string->er s)
  (define r (string->number s))
  ;; what is the predicate for Exact-Rational ???
  (and r (if (real? r) (cast r Exact-Rational) #f)))

;; a bi-dorectional temperature converter (Fahrenheit vs Celsius]

(define-type Temp Exact-Rational)
(define-type CB   {(Instance Text-Field%) Any -> Void})

(define *C : Temp 0)
(define *F : Temp 0)

(: callback ((Temp -> Void) -> CB))
(define ((callback setter) field _evt)
  (send field set-field-background (make-object color% "white"))
  (define field:num (string->er (send field get-value)))
  (if field:num (setter field:num) (send field set-field-background (make-object color% "red"))))

(define-syntax-rule (flow *from --> *to to-field)
  (λ ({x : Temp})
    (set!-values (*from *to) (values x (--> x)))
    (send to-field set-field-background (make-object color% "white"))
    (send to-field set-value (~r *to #:precision 4))))

(define celsius->fahrenheit : CB (callback (flow *C (λ ({c : Temp}) (+  (* c 9/5) 32)) *F F-field)))
(define fahrenheit->celsius : CB (callback (flow *F (λ ({f : Temp}) (* (- f 32) 5/9))  *C C-field)))

(define frame   (new frame% [label "temperature converter"]))
(define pane    (new horizontal-pane% [parent frame]))
(define (field {v0 : String} {lbl : String} (cb : CB)) : (Instance Text-Field%)
  (new text-field% [parent pane][min-width 199][label lbl][init-value v0][callback cb]))
(define C-field (field  "0" "celsius:"       celsius->fahrenheit))
(define F-field (field  "0" " = fahrenheit:" fahrenheit->celsius))

(celsius->fahrenheit C-field 'start-me-up)
(send frame show #t)
