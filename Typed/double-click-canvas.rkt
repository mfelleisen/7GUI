#lang typed/racket/gui

(provide canvas-double-click%)

(require 7GUI/Typed/sub-canvas)

(define-type-canvas Canvas-Double-Click%
  (vert-margin (->* () (Integer) Void))
  (horiz-margin (->* () (Integer) Void))
  (get-scaled-client-size (-> (Values Integer Integer)))
  (get-gl-client-size (-> (Values Integer Integer)))

  (on-click (-> Natural Natural Void))
  (on-double-click (-> Natural Natural Void))

  [augment (on-event  (-> (Instance Mouse-Event%) Void))]

  [augment (on-click (-> Natural Natural Void))]
  [augment (on-double-click (-> Natural Natural Void))])

(require/typed 7GUI/canvas-double-click [canvas-double-click% Canvas-Double-Click%])
