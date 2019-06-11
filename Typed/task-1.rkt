#! /usr/bin/env gracket
#lang typed/racket/gui

;; a mouse-click counter

(define *counter 0)

(: count! (-> Any * Void))
(define (count! . x)
  (set! *counter (if (empty? x)  0 (+ *counter 1)))
  (send view set-value (~a *counter)))

(define frame (new frame% [label "Counter"]))
(define pane  (new horizontal-pane% [parent frame]))
(define view  (new text-field% [parent pane][label ""][init-value "0"][enabled #f][min-width 100]))
(define _but  (new button% [parent pane] [label "Count"] [callback count!]))

(count!)
(send frame show #t)
