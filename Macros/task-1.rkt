#! /usr/bin/env gracket
#lang racket/gui

(require 7GUI/Macros/7guis)

(define-state *count 0 (lambda (x) (send display set-value (~a x))))

(gui "Counter" 
     ((#:id display text-field% [label ""][init-value "0"][enabled #f][min-width 100])
      (button% #:change *count (just add1) [label "Count"])))