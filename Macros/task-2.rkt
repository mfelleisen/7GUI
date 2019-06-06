#! /usr/bin/env gracket
#lang racket/gui

;; a bi-dorectional temperature converter (Fahrenheit vs Celsius]

(require 7GUI/Macros/7guis)

(define ((callback flow*) field _evt)
  (define field:num (string->number (send field get-value)))
  (send field set-field-background (make-object color% "white"))
  (if field:num (flow* field:num) (send field set-field-background (make-object color% "red"))))

(define-syntax-rule (flow *from --> *to)
  (λ (x) (unless (= *from x) (set!-values (*from *to) (values x (--> x))))))

(define celsius->fahrenheit (callback (flow *C (λ (c) (+ (* c 9/5) 32)) *F)))
(define fahrenheit->celsius (callback (flow *F (λ (f) (* (- f 32) 5/9)) *C)))

(define temp-field% (class text-field% (super-new [min-width 200])))

(gui "Temperature Converter" 
     ((#:id F-field temp-field% [init-value "32"][label "fahrenheit:"][callback fahrenheit->celsius])
      (#:id C-field temp-field% [init-value "0"][label "celsius:"][callback celsius->fahrenheit])))

(define ((propagate-to field) x)
  (send field set-field-background (make-object color% "white"))
  (send field set-value (~r x #:precision 4)))

(define-state* (*C 0 (propagate-to C-field)) (*F 32 (propagate-to F-field)))