#lang typed/racket/gui

(provide
 ;; SYNTAX
 #; (define-type-canvas TypeName% [#:minus-init (init-param:id)] method-spec ...)
 #; {method-spec = name+type || (augment . name+type)}
 ;; creates a sub-type Class specification for a canvas,
 ;; -- subtracting init-paameters as init-param ... from those in Canva%
 ;; -- adding method specifications method-spec ...

 define-type-canvas)

;; ---------------------------------------------------------------------------------------------------
(require 7GUI/Typed/sub)

;; ---------------------------------------------------------------------------------------------------
(define-sub-type define-type-canvas Canvas%
  (parent         (Instance Area-Container<%>))
  (style          Style* #:optional)
  (paint-callback (-> (Instance Canvas%) (Instance DC<%>) Any) #:optional)
  (label          MaybeString #:optional)
  (gl-config      Any #:optional)
  (enabled        Any #:optional)
  (vert-margin    Nonnegative-Integer #:optional)
  (horiz-margin   Nonnegative-Integer #:optional)
  (min-width      MaybeN #:optional)
  (min-height     MaybeN #:optional)
  (stretchable-width  Any #:optional)
  (stretchable-height Any #:optional))

(define-type Style
  (U 'combo
     'border 'control-border 'gl 'hscroll 'vscroll 'resize-corner
     'deleted 'no-autoclear 'no-focus
     'transparent))

(define-type Style* (Listof Style))