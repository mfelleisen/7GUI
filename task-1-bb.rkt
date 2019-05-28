#lang at-exp racket/gui

;; count the number of clicks on the "count" button

(require 2htdp/universe 2htdp/image)

(big-bang 0
  (to-draw (lambda (c) (text (format "   ~a   Count   " c) 22 'red)))
  (on-mouse (lambda (c _x _y me) (if (mouse=? "button-down" me) (+ c 1) c))))