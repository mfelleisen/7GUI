#lang racket/gui

;; a mouse-click counter

(define counter-state 0)

(define (count! . x)
  (if (empty? x) (set! counter-state 0) (set! counter-state (+ counter-state 1)))
  (send view set-label (~a counter-state)))

(define frame  (new frame% [label "Counter"]))
(define view   (new button% [parent frame] [label ""] [callback void]))
(define button (new button% [parent frame] [label "Count"] [callback count!]))

(count!)
(send frame show #t)