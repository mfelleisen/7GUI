#lang at-exp typed/racket/gui

(provide sub-Frame)

(define-syntax-rule (sub-Frame Adjuster-Dialog% circle)
  (define-type Adjuster-Dialog%
    (Class #:implements Frame%
     (continuous (-> Natural Void))
     (init-field {closest-circle circle})
     
     (init 
      (label String)
      (parent (U (Instance Frame%) False) #:optional)
      (width (U False Integer) #:optional)
      (height (U False Integer) #:optional)
      (x (U False Integer) #:optional)
      (y (U False Integer) #:optional)
      (style
       (Listof
        (U 'float
           'fullscreen-aux
           'fullscreen-button
           'hide-menu-bar
           'metal
           'no-caption
           'no-resize-border
           'no-system-menu
           'toolbar-button))
       #:optional)
      (enabled Any #:optional)
      (border Nonnegative-Integer #:optional)
      (spacing Nonnegative-Integer #:optional)
      (alignment
       (List (U 'center 'left 'right) (U 'bottom 'center 'top))
       #:optional)
      (min-width (U Exact-Nonnegative-Integer False) #:optional)
      (min-height (U Exact-Nonnegative-Integer False) #:optional)
      (stretchable-width Any #:optional)
      (stretchable-height Any #:optional)))))