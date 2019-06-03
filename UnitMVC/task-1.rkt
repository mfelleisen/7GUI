#! /usr/bin/env gracket
#lang racket/gui

;; a mouse-click counter

;; ---------------------------------------------------------------------------------------------------
;; the agreement between model and view 

(define-signature count^ (count!))
(define-signature run^ (run on-change-to-model))

;; ---------------------------------------------------------------------------------------------------
;; the model

(define count@
  (unit
    (import run^)
    (export count^)
    [define *counter 0]
    (define (count! . _)
      (set! *counter (+ *counter 1))
      (on-change-to-model *counter))))
  
;; ---------------------------------------------------------------------------------------------------
;; a function for generating two views

(define (view@ R)
  (unit
    (import count^)
    (export run^)
    (init-depend count^)

    (define f (new frame% [label "Counter"]))
    (define p  (new horizontal-pane% [parent f]))
    (define v  (new text-field% [parent p][label ""][init-value (R 0)][enabled #f][min-width 99]))
    (define _  (new button% [parent p] [label "Count"] [callback count!]))

    (define (on-change-to-model c) (send v set-value (R c)))
    (define (run) (send f show #t))))

;; ---------------------------------------------------------------------------------------------------
;; a function for generating a view different and linking it to the model 

(define (run@ rendering)
  (compound-unit
    (import)
    (export view)
    (link
     (((count : count^)) count@ view)
     (((view  : run^))  (view@ rendering) count))))

;; ---------------------------------------------------------------------------------------------------
;; let's run them 

(define (run-a)
  (define-values/invoke-unit (run@ ~a) (import) (export run^))
  (run))

(define (run-sticks)
  (define (nat->sticks n) (make-string n #\|))
  (define-values/invoke-unit (run@ nat->sticks) (import) (export run^))
  (run))

(run-a)
(run-sticks)
