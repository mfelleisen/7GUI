#lang at-exp typed/racket/gui

(provide define-type-frame)

;; ---------------------------------------------------------------------------------------------------
(require 7GUI/Typed/sub)

;; ---------------------------------------------------------------------------------------------------
(define-sub-type define-type-frame Frame%
  (parent (U (Instance Frame%) False) #:optional)
  (width (U False Integer) #:optional)
  (height (U False Integer) #:optional)
  (x (U False Integer) #:optional)
  (y (U False Integer) #:optional)
  (style Style* #:optional)
  (enabled Any #:optional)
  (border Nonnegative-Integer #:optional)
  (spacing Nonnegative-Integer #:optional)
  (alignment (List (U 'center 'left 'right) (U 'bottom 'center 'top)) #:optional)
  (min-width (U Exact-Nonnegative-Integer False) #:optional)
  (min-height (U Exact-Nonnegative-Integer False) #:optional)
  (stretchable-width Any #:optional)
  (stretchable-height Any #:optional))

(define-type Style
  (U 'float 'toolbar-button 'fullscreen-aux 'fullscreen-button 'hide-menu-bar
     'metal 'no-caption 'no-resize-border 'no-system-menu))

(define-type Style* (Listof Style))