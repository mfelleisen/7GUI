#! /usr/bin/env gracket
#lang racket/gui

;; a bi-dorectional temperature converter (Fahrenheit vs Celsius]

(require 7GUI/Macros/7guis)

(define-syntax-rule (propagate-to state f field)
  (λ (new-value-of-origin)
    (set! state (stop (f new-value-of-origin)))
    (send field set-field-background (make-object color% "white"))
    (send field set-value (~r state #:precision 4))))

(define-state *C 0  (propagate-to *F (λ (c) (+ (* c 9/5) 32)) F-field))
(define-state *F 32 (propagate-to *C (λ (f) (* (- f 32) 5/9)) C-field))

(define-syntax-rule (from field)
  (λ (old)
    (define field:num (string->number (send field get-value)))
    (send field set-field-background (make-object color% "white"))
    (or field:num (begin (send field set-field-background (make-object color% "red")) none))))

(define temp-field% (class text-field% (super-new [min-width 200])))
    
(gui "Temperature Converter" 
     ((#:id F-field temp-field% #:change *F (from F-field) [init-value "32"][label "fahrenheit:"])
      (#:id C-field temp-field% #:change *C (from C-field) [init-value "0"][label "celsius:"])))