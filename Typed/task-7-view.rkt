#lang typed/racket/gui

(provide WIDTH HEIGHT paint-grid xy->A0)

(require (only-in 7GUI/Typed/task-7-exp Content Ref))

(require/typed 7GUI/task-7-view
               [WIDTH      Natural]
               [HEIGHT     Natural]
               [paint-grid (-> (Instance DC<%>) Content Void)]
	       [xy->A0     (-> Natural Natural Ref)])



