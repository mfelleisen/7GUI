#lang typed/racket/gui

(provide WIDTH HEIGHT paint-grid x->A y->0)

(require (only-in 7GUI/Typed/task-7-exp Content Letter))

(require/typed 7GUI/task-7-view
               [WIDTH Natural]
               [HEIGHT Natural]
               [paint-grid (-> (Instance DC<%>) Content Void)]
               [x->A (Natural -> Letter)]
               [y->0 (Natural -> Index)])
