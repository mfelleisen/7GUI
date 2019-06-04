#lang racket/gui

(require 7GUI/Macros/7guis)

(define (inc-counter . x) (set! *count (+ 1 *count)))

(gui "Counter"
      (*count 0 (lambda (x) (send display set-value (~a x))))
      ((#:id display text-field% [label ""][init-value "0"][enabled #f][min-width 100])
       (button% [label "Count"][callback inc-counter])))