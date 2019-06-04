#! /usr/bin/env gracket
#lang racket/gui

;; a bi-dorectional temperature converter (Fahrenheit vs Celsius]

(require 7GUI/Macros/7guis)

(define-syntax-rule (callback setter)
  (λ (field _evt)
    (define field:num (string->number (send field get-value)))
    (if field:num (setter field:num) (send field set-field-background (make-object color% "red")))))

(define-syntax-rule (flow *from --> *to) (λ (x) (set!-values (*from *to) (values x (--> x)))))

(define ((propagate to-field) x)
  (send to-field set-field-background (make-object color% "white"))
  (send to-field set-value (~r x #:precision 4)))

(define celsius->fahrenheit (callback (flow *C (λ (c) (+  (* c 9/5) 32)) *F)))
(define fahrenheit->celsius (callback (flow *F (λ (f) (* (- f 32) 5/9))  *C)))

(define temp-field% (class text-field% (super-new [min-width 200])))

(gui "Temperature Converter" {(*C 0 (propagate C-field)) (*F 32 (propagate F-field))}
     ((#:id F-field temp-field% [init-value "32"][label "fahrenheit:"][callback fahrenheit->celsius])
      (#:id C-field temp-field% [init-value "0"][label "celsius:"][callback celsius->fahrenheit])))