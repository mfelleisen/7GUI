#lang racket/gui

;; a mouse-click counter

(define counter-state 0)

(define (count! . x)
  (if (empty? x) (set! counter-state 0) (set! counter-state (+ counter-state 1)))
  (send view set-value (~a counter-state)))

(define frame  (new frame% [label "Counter"]))
(define pane   (new horizontal-pane% [parent frame]))
(define view   (new text-field% [parent pane] [label ""] [init-value "0"][enabled #f][min-width 40]))
(define button (new button% [parent pane] [label "Count"] [callback count!]))

(count!)
(send frame show #t)
