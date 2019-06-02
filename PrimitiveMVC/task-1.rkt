#! /usr/bin/env gracket
#lang racket/gui

;; a mouse-click counter; use (count!) or (count! 'x) at REPL and thus change the state of the model 

;; ---------------------------------------------------------------------------------------------------
(define count! ;; depends on on-change-to-model 
  (let ([*counter 0])
    (λ x 
      (set! *counter (if (empty? x)  0 (+ *counter 1)))
      (on-change-to-model *counter))))

;; ---------------------------------------------------------------------------------------------------
(define frame (new frame% [label "Counter"]))
(define pane  (new horizontal-pane% [parent frame]))
(define view  (new text-field% [parent pane][label ""][init-value "0"][enabled #f][min-width 100]))
(define _but  (new button% [parent pane] [label "Count"] [callback count!]))

(define (on-change-to-model c) (send view set-value (~a c)))

;; ---------------------------------------------------------------------------------------------------
(count!)
(send frame show #t)