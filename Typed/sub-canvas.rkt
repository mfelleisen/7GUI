#lang typed/racket/gui

(provide Sub-Canvas%)

(define-syntax-rule (Sub-Canvas% Circle-Canvas% circle)
  (define-type Circle-Canvas%
    (Class
     (unlock (-> Void))
     (draw-circles (->* ( {U False circle} ) ( [U False (Listof circle)]) Void))
     (on-event (-> (Instance Mouse-Event%) Void))
         
     (init (parent (Instance Area-Container<%>))
           (style
            (Listof
             (U 'border
                'combo
                'control-border
                'deleted
                'gl
                'hscroll
                'no-autoclear
                'no-focus
                'resize-corner
                'transparent
                'vscroll))
            #:optional)
           #;
           (paint-callback (-> (Instance Canvas%) (Instance DC<%>) Any) #:optional)
           (label (U False String) #:optional)
           (gl-config Any #:optional)
           (enabled Any #:optional)
           (vert-margin Nonnegative-Integer #:optional)
           (horiz-margin Nonnegative-Integer #:optional)
           (min-width (U Exact-Nonnegative-Integer False) #:optional)
           (min-height (U Exact-Nonnegative-Integer False) #:optional)
           (stretchable-width Any #:optional)
           (stretchable-height Any #:optional))
     (accept-drop-files (case-> (-> Boolean) (-> Any Void)))
     (accept-tab-focus (case-> (-> Boolean) (-> Any Void)))
     (client->screen (-> Integer Integer (values Integer Integer)))
     (enable (-> Any Void))
     (flush (-> Void))
     (focus (-> Void))
     (get-canvas-background (-> (U (Instance Color%) False)))
     (get-client-handle (-> Any))
     (get-client-size (-> (values Nonnegative-Integer Nonnegative-Integer)))
     (get-cursor (-> (U (Instance Cursor%) False)))
     (get-dc (-> (Instance DC<%>)))
     (get-graphical-min-size (-> (values Nonnegative-Integer Nonnegative-Integer)))
     (get-handle (-> Any))
     (get-height (-> Nonnegative-Integer))
     (get-label
      (-> (U 'app
             'caution
             'stop
             (Instance Bitmap%)
             (List (Instance Bitmap%) String (U 'bottom 'left 'right 'top))
             False
             String)))
     (get-parent (-> (U (Instance Area-Container<%>) False)))
     (get-plain-label (-> (U False String)))
     (get-scroll-page (-> (U 'horizontal 'vertical) Positive-Integer))
     (get-scroll-pos (-> (U 'horizontal 'vertical) Positive-Integer))
     (get-scroll-range (-> (U 'horizontal 'vertical) Positive-Integer))
     (get-size (-> (values Nonnegative-Integer Nonnegative-Integer)))
     (get-top-level-window (-> (Instance Top-Level-Window<%>)))
     (get-view-start (-> (values Nonnegative-Integer Nonnegative-Integer)))
     (get-virtual-size (-> (values Nonnegative-Integer Nonnegative-Integer)))
     (get-width (-> Nonnegative-Integer))
     (get-x (-> Integer))
     (get-y (-> Integer))
     (has-focus? (-> Boolean))
     (init-auto-scrollbars
      (-> (U Exact-Nonnegative-Integer False)
          (U Exact-Nonnegative-Integer False)
          Real
          Real
          Void))
     (init-manual-scrollbars
      (-> (U Exact-Nonnegative-Integer False)
          (U Exact-Nonnegative-Integer False)
          Positive-Integer
          Positive-Integer
          Nonnegative-Integer
          Nonnegative-Integer
          Void))
     (is-enabled? (-> Boolean))
     (is-shown? (-> Boolean))
     (make-bitmap (-> Positive-Integer Positive-Integer (Instance Bitmap%)))
     (min-client-height
      (case-> (-> Nonnegative-Integer) (-> Nonnegative-Integer Void)))
     (min-client-width
      (case-> (-> Nonnegative-Integer) (-> Nonnegative-Integer Void)))
     (min-height (case-> (-> Integer) (-> Integer Void)))
     (min-width (case-> (-> Integer) (-> Integer Void)))
     (on-char (-> (Instance Key-Event%) Void))
     (on-drop-file (-> Path Void))
   
     (on-focus (-> Any Void))
     (on-move (-> Integer Integer Void))
     (on-paint (-> Void))
     (on-scroll (-> Any Void))
     (on-size (-> Nonnegative-Integer Nonnegative-Integer Void))
     (on-subwindow-char (-> (Instance Window<%>) (Instance Key-Event%) Boolean))
     (on-subwindow-event (-> (Instance Window<%>) (Instance Mouse-Event%) Boolean))
     (on-subwindow-focus (-> (Instance Window<%>) Any Void))
     (on-superwindow-enable (-> Any Void))
     (on-superwindow-show (-> Any Void))
     (on-tab-in (-> Void))
     (popup-menu
      (-> (Instance Popup-Menu%) Nonnegative-Integer Nonnegative-Integer Void))
     (refresh (-> Void))
     (refresh-now (->* () ((-> (Instance DC<%>) Any) #:flush? Any) Void))
     (reparent (-> (Instance Pane%) Void))
     (resume-flush (-> Void))
     (screen->client (-> Integer Integer (values Integer Integer)))
     (scroll (-> (U False Real) (U False Real) Void))
     (set-canvas-background (-> (Instance Color%) Void))
     (set-cursor (-> (U (Instance Cursor%) False) Void))
     (set-label (-> String Void))
     (set-resize-corner (-> Any Void))
     (set-scroll-page (-> (U 'horizontal 'vertical) Positive-Integer Void))
     (set-scroll-pos (-> (U 'horizontal 'vertical) Positive-Integer Void))
     (set-scroll-range (-> (U 'horizontal 'vertical) Positive-Integer Void))
     (show (-> Any Void))
     (show-scrollbars (-> Any Any Void))
     (stretchable-height (case-> (-> Boolean) (-> Any Void)))
     (stretchable-width (case-> (-> Boolean) (-> Any Void)))
     (suspend-flush (-> Void))
     (swap-gl-buffers (-> Void))
     (warp-pointer (-> Integer Integer Void))
     (with-gl-context (-> (-> Any) [#:fail (-> Any)] Any)))))
