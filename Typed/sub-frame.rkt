#lang at-exp typed/racket/gui

(provide define-type-frame)

;; ---------------------------------------------------------------------------------------------------
(require 7GUI/Typed/sub)

;; ---------------------------------------------------------------------------------------------------
(define-sub-type define-type-frame Frame%
  (parent     (U (Instance Frame%) False) #:optional)
  (label      String)
  ;; if I bring this line in, I get a weird error message: 
  #;
  (width      (U False Natural) #:optional)
  ;; Type Checker: type mismatch;
  ;; wrong type for init `width'
  ;; expected: temp9
  ;; given: temp6 
  (width      MaybeInt #:optional)
  (height     MaybeInt #:optional)
  (x          MaybeInt #:optional)
  (y          MaybeInt #:optional)
  (style      Style* #:optional)
  (enabled    Any #:optional)
  (border     Nonnegative-Integer #:optional)
  (spacing    Nonnegative-Integer #:optional)
  (alignment  Alignment #:optional)
  (min-width  MaybeN #:optional)
  (min-height MaybeN #:optional)
  (stretchable-width  Any #:optional)
  (stretchable-height Any #:optional))

(define-type Alignment (List (U 'center 'left 'right) (U 'bottom 'center 'top)))

(define-type Style
  (U 'float 'toolbar-button 'fullscreen-aux 'fullscreen-button 'hide-menu-bar
     'metal 'no-caption 'no-resize-border 'no-system-menu))

(define-type Style* (Listof Style))