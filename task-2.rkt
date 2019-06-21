#! /usr/bin/env gracket
#lang racket/gui

;; a bi-directional temperature converter (Fahrenheit vs Celsius)

(define *C 0)
(define *F 0)

(define ((callback setter) field _evt)
  (define-values (field:num last) (string->number* (send field get-value)))
  (cond
    [(and field:num (rational? field:num))
     (define inexact-n (* #i1.0 field:num))
     (setter inexact-n)
     (render field inexact-n last)]
    [else (send field set-field-background (make-object color% "red"))]))

(define (string->number* str)
  (define n (string->number str))
  (values n (and n (string-ref str (- (string-length str) 1)))))

(define-syntax-rule (flow *from --> *to to-field)
  (λ (x)
    (set!-values (*from *to) (values x (--> x)))
    (render to-field *to "")))

(define (render to-field *to last)
  (send to-field set-field-background (make-object color% "white"))
  (send to-field set-value (~a (~r *to #:precision 4) (if (eq? #\. last) "." ""))))

(define celsius->fahrenheit (callback (flow *C (λ (c) (+  (* c 9/5) 32)) *F F-field)))
(define fahrenheit->celsius (callback (flow *F (λ (f) (* (- f 32) 5/9))  *C C-field)))

(define frame   (new frame% [label "temperature converter"]))
(define pane    (new horizontal-pane% [parent frame]))
(define (field v0 lbl cb)
  (new text-field% [parent pane][min-width 199][label lbl][init-value v0][callback cb]))
(define C-field (field  "0" "celsius:"       celsius->fahrenheit))
(define F-field (field  "0" " = fahrenheit:" fahrenheit->celsius))

(celsius->fahrenheit C-field 'start-me-up)
(send frame show #t)
