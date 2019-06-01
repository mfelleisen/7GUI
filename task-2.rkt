#! /usr/bin/env gracket
#lang racket/gui

;; a bi-dorectional temperature converter (Fahrenheit vs Celsius]

(define *C 0)
(define *F 0)

(define-syntax-rule (flow *from --> *to) (λ (x) (set!-values (*from *to) (values x (--> x)))))

(define ((callback setter) . self+evt)
  (define field:num (if (empty? self+evt) 0 (string->number (send (first self+evt) get-value))))
  (when field:num
    (setter field:num)
    (send C-field set-value (~a *C))
    (send F-field set-value (~a *F))))

(define (field lbl cb) (new text-field% [parent pane] [label lbl] [init-value ""] [callback cb]))

(define celsius->fahrenheit (callback (flow *C (λ (c) (+ (* c 9/5) 32)) *F)))
(define fahrenheit->celsius (callback (flow *F (λ (f) (* (- f 32) 5/9)) *C)))

(define frame   (new frame% [label "temperature converter"]))
(define pane    (new horizontal-pane% [parent frame]))
(define C-field (field "celsius:"       celsius->fahrenheit))
(define F-field (field " = fahrenheit:" fahrenheit->celsius))

(celsius->fahrenheit)
(send frame show #t)