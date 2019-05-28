#lang racket/gui

;; a bi-dorectional temperature converter (Fahrenheit vs Celsius]

(define *C 0)
(define *F 0)

(define-syntax-rule
  (define-callback (name the-field *from converter *to))
  (define (name . x)
    (define field-as-number (if (empty? x) 0 (string->number (send the-field get-value))))
    (when field-as-number 
      (set! *from field-as-number)
      (set! *to   (converter *from))
      (send C-field set-value (~a *C))
      (send F-field set-value (~a *F)))))

(define (field lbl cb)
  (new text-field% [parent pane] [label lbl] [init-value ""] [callback cb]))

(define-callback (celsius->fahrenheit C-field    *C (λ (c) (+ (* c 9/5) 32)) *F))
(define-callback (fahrenheit->celsius F-field *F (λ (f) (* (- f 32) 5/9)) *C))

(define frame   (new frame% [label "temperature converter"]))
(define pane    (new horizontal-pane% [parent frame]))
(define C-field (field "celsius:" celsius->fahrenheit))
(define F-field (field " = fahrenheit:" fahrenheit->celsius))

(celsius->fahrenheit)
(send frame show #t)