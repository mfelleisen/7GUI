#! /usr/bin/env gracket
#lang typed/racket/gui

;; a circle drawer with undo/redo facilities (unclear spec for resizing)

;; ---------------------------------------------------------------------------------------------------
(define Default-Diameter 20)

(define-type Action (U Symbol [List Symbol [Listof Natural]]))
(struct circle ({x : Integer} {y : Integer} {d : Natural} {action : Action}) #:transparent)

(: draw-1-circle (-> (Instance DC<%>) (Instance Brush%) circle Void))
(define (draw-1-circle dc brush c)
  (send dc set-brush brush)
  (match-define (circle x y d _a) c)
  (send dc draw-ellipse x y d d))

(: *circles [Listof circle])
(define *circles '())

(: *history [Listof circle])
(define *history '())

(: add-circle! (-> Integer Integer Void))
(define (add-circle! x y)
  (define added (circle x y Default-Diameter 'added))
  (set! *circles (cons added *circles)))

(: resize! (-> circle Natural Void))
(define (resize! old-closest new-d)
  (match-define (circle x y d a) old-closest)
  (define resized
    (match a
      ['added (circle x y new-d `(resized (,d)))]
      [`(resized ,old-sizes) (circle x y new-d `(resized ,(cons d old-sizes)))]))
  (set! *circles (cons resized (remq old-closest *circles))))

(define (undo) : Void 
  (when (cons? *circles)
    (define fst (first *circles))
    (match fst
      [(circle x y d 'added) (set! *circles (rest *circles))]
      [(circle x y d `(resized (,r0 . ,sizes)))
       (set! *circles (cons (circle x y r0 `(resized (,d))) (rest *circles)))])
    (set! *history (cons fst *history))))

(define (redo) : Void 
  (when (cons? *history)
    (define fst (first *history))
    (if (eq? (circle-action fst) 'added)
        (set!-values (*circles *history) (values (cons fst *circles) (rest *history)))
        (set!-values (*circles *history) (values (cons fst (rest *circles)) (rest *history))))))

(: the-closest (->* (Integer Integer) ([Listof circle]) circle))
(define (the-closest xm ym (circles *circles))
  (argmin (distance xm ym) circles))

(: is-empty-area (->* (Integer Integer) ([Listof circle]) Boolean))
(define (is-empty-area xm ym (circles *circles))
  (define dist (distance xm ym))
  (for/and ((c circles)) (> (dist c) (/ (+ (circle-d c) Default-Diameter) 2))))

(: distance (-> Integer Integer (-> circle Real)))
(define ((distance xm ym) c)
  (match-define (circle xc yc _d _a) c)
  (sqrt (+ (sqr (- xc xm)) (sqr (- yc ym)))))

;; ---------------------------------------------------------------------------------------------------
(define solid-gray  (new brush% [color "gray"]))
(define white-brush (new brush% [color "white"]))

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
   (with-gl-context (-> (-> Any) [#:fail (-> Any)] Any))))
(define circle-canvas% : Circle-Canvas% 
  (class canvas% 
    (define *in-adjuster : Boolean #f) ;; we can get a quasi-modal dialog this way 
    (define/public (unlock) : Void (set! *in-adjuster #f))
    (define/private (lock) : Void (set! *in-adjuster #t))

    (define *x : (U False Integer) 0)
    (define *y 0)

    (define/override (on-event {evt : (Instance Mouse-Event%)}) : Void 
      (unless *in-adjuster
        (define type (send evt get-event-type))
        (set! *x (send evt get-x))
        (set! *y (send evt get-y))
        (cond
          [(eq? 'leave type) (set! *x #f)]
          [(eq? 'enter type) (set! *x 0)]
          [(and (eq? 'left-down type) (is-empty-area (cast *x Integer) *y)) (add-circle! (cast *x Integer) *y)]
          [(and (eq? 'right-down type) (cons? *circles)) (lock) (popup-adjuster (the-closest (cast *x Integer) *y))])
        (send this on-paint)))
    
    (define/public (draw-circles {closest : (U False circle)} (others-without-closest : (U False [Listof circle]) #f)) : Void
      (define dc : (Instance DC<%>) (send (cast this (Instance Canvas%)) get-dc))
      (send dc clear)
      (for ((c : circle (in-list (if (cons? others-without-closest) others-without-closest *circles)))) (draw-1-circle dc white-brush c))
      (when closest (draw-1-circle dc solid-gray closest)))
    
    (define (paint-callback {_self : (Instance Canvas%)} {dc : (Instance DC<%>)}) : Any 
      (cond
        [(empty? *circles) (send dc clear)]
        [(boolean? *x)     (draw-circles #f)]
        [else              (draw-circles (the-closest (cast *x Integer) *y))]))
    
    (super-new [paint-callback paint-callback])))


(: popup-adjuster (-> circle Void))
(define (popup-adjuster closest-circle)
  (define (cb {_ : Any} {evt : (Instance Control-Event%)}) : Void 
    (when (eq? (send evt get-event-type) 'menu-popdown-none) (send canvas unlock)))
  (define pm (new popup-menu% [title "adjuster"][popdown-callback cb]))
  (new menu-item% [parent pm] [label "adjust radius"] [callback (adjuster! closest-circle)])
  (send frame popup-menu pm  100 100))

(: adjuster! (-> circle (->* () () #:rest Any Void)))
(define ((adjuster! closest-circle) . x)
  (define d0 (circle-d closest-circle))
  (define frame (new adjuster-dialog% [label "to make type checker happy"][closest-circle closest-circle]))
  (new adjuster-slider% [parent frame][init-value d0][update (λ ({x : Natural}) (send frame continuous x))])
  (send frame show #t))

(define-type Adjuster-Dialog%
  (Class
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
         (stretchable-height Any #:optional))
   (accept-drop-files (case-> (-> Boolean) (-> Any Void)))
   (add-child (-> (Instance Subwindow<%>) Void))
   (after-new-child (-> (Instance Subarea<%>) Void))
   (begin-container-sequence (-> Void))
   (border (case-> (-> Nonnegative-Integer) (-> Nonnegative-Integer Void)))
   (can-close? (-> Boolean))
   (can-exit? (-> Boolean))
   (center (-> (U 'both 'horizontal 'vertical) Void))
   (change-children
    (-> (-> (Listof (Instance Subarea<%>)) (Listof (Instance Subarea<%>))) Void))
   (client->screen (-> Integer Integer (values Integer Integer)))
   (container-flow-modified (-> Void))
   (container-size
    (-> (Listof (List Nonnegative-Integer Nonnegative-Integer Any Any))
        (values Nonnegative-Integer Nonnegative-Integer)))
   (create-status-line (-> Void))
   (delete-child (-> (Instance Subwindow<%>) Void))
   (display-changed (-> Any))
   (enable (-> Any Void))
   (end-container-sequence (-> Void))
   (focus (-> Void))
   (fullscreen (-> Any Void))
   (get-alignment
    (-> (values (U 'center 'left 'right) (U 'bottom 'center 'top))))
   (get-children (-> (Listof (Instance Subarea<%>))))
   (get-client-handle (-> Any))
   (get-client-size (-> (values Nonnegative-Integer Nonnegative-Integer)))
   (get-cursor (-> (U (Instance Cursor%) False)))
   (get-edit-target-object
    (-> (U (Instance Editor<%>) (Instance Window<%>) False)))
   (get-edit-target-window (-> (U (Instance Window<%>) False)))
   (get-eventspace (-> Any))
   (get-focus-object (-> (U (Instance Editor<%>) (Instance Window<%>) False)))
   (get-focus-window (-> (U (Instance Window<%>) False)))
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
   (get-menu-bar (-> (U (Instance Menu-Bar%) False)))
   (get-parent (-> (U (Instance Area-Container<%>) False)))
   (get-plain-label (-> (U False String)))
   (get-size (-> (values Nonnegative-Integer Nonnegative-Integer)))
   (get-top-level-window (-> (Instance Top-Level-Window<%>)))
   (get-width (-> Nonnegative-Integer))
   (get-x (-> Integer))
   (get-y (-> Integer))
   (has-focus? (-> Boolean))
   (has-status-line? (-> Boolean))
   (iconize (-> Any Void))
   (is-enabled? (-> Boolean))
   (is-fullscreened? (-> Boolean))
   (is-iconized? (-> Boolean))
   (is-maximized? (-> Boolean))
   (is-shown? (-> Boolean))
   (maximize (-> Any Void))
   (min-height (case-> (-> Integer) (-> Integer Void)))
   (min-width (case-> (-> Integer) (-> Integer Void)))
   (modified (case-> (-> Boolean) (-> Any Void)))
   (move (-> Integer Integer Void))
   (on-activate (-> Any Void))
   (on-close (-> Void))
   (on-drop-file (-> Path Void))
   (on-exit (-> Void))
   (on-focus (-> Any Void))
   (on-menu-char (-> (Instance Key-Event%) Boolean))
   (on-message (-> Any Void))
   (on-move (-> Integer Integer Void))
   (on-size (-> Nonnegative-Integer Nonnegative-Integer Void))
   (on-subwindow-char (-> (Instance Window<%>) (Instance Key-Event%) Boolean))
   (on-subwindow-event (-> (Instance Window<%>) (Instance Mouse-Event%) Boolean))
   (on-subwindow-focus (-> (Instance Window<%>) Any Void))
   (on-superwindow-enable (-> Any Void))
   (on-superwindow-show (-> Any Void))
   (on-system-menu-char (-> (Instance Key-Event%) Boolean))
   (on-toolbar-button-click (-> Void))
   (on-traverse-char (-> (Instance Key-Event%) Boolean))
   (place-children
    (-> (Listof (List Nonnegative-Integer Nonnegative-Integer Any Any))
        Nonnegative-Integer
        Nonnegative-Integer
        (Listof
         (List
          Nonnegative-Integer
          Nonnegative-Integer
          Nonnegative-Integer
          Nonnegative-Integer))))
   (popup-menu
    (-> (Instance Popup-Menu%) Nonnegative-Integer Nonnegative-Integer Void))
   (reflow-container (-> Void))
   (refresh (-> Void))
   (resize (-> Integer Integer Void))
   (screen->client (-> Integer Integer (values Integer Integer)))
   (set-alignment (-> (U 'center 'left 'right) (U 'bottom 'center 'top) Void))
   (set-cursor (-> (U (Instance Cursor%) False) Void))
   (set-icon
    (->* ((Instance Bitmap%)) ((Instance Bitmap%) (U 'both 'large 'small)) Void))
   (set-label (-> String Void))
   (set-status-text (-> String Void))
   (show (-> Any Void))
   (spacing (case-> (-> Nonnegative-Integer) (-> Nonnegative-Integer Void)))
   (stretchable-height (case-> (-> Boolean) (-> Any Void)))
   (stretchable-width (case-> (-> Boolean) (-> Any Void)))
   (warp-pointer (-> Integer Integer Void))
   (augment
    (can-close? (-> Boolean))
    (display-changed (-> Any))
    (on-close (-> Void)))))


;; TO BE TYPED, work around bugs in Typed Racket 
(define adjuster-dialog% : Adjuster-Dialog% 
  (class frame% (init-field closest-circle)
    ;; to make type checker happy
    (init label)
    ;; the next 3 are needed to get rid of error that says missing type for closest-circle
    (: x* Integer)
    (: y* Integer)
    (: *d Natural)
    (match-define (circle x* y* *d _) closest-circle)
 
    (: others (Listof circle))
    (define others (remq closest-circle *circles))
    
    (define/public (continuous {new-d : Natural}) : Void ;; resize locally while adjusting 
      (set! *d new-d)
      (send canvas draw-circles (circle x* y* *d '_dummy_) others))
    
    (define/augment (on-close) : Void ;; resize globally 
      (send canvas unlock)
      (resize! closest-circle *d))

    (super-new [label (format "Adjust radius of circle at (~a,~a)" x* y*)])))

(define adjuster-slider%
  (class slider% (init-field {update : (Natural -> Void)})
    (inherit get-value)
    (super-new [label ""][min-value 10][max-value 100][callback (λ _ (update (cast (get-value) Natural)))])))

;; ---------------------------------------------------------------------------------------------------
(define frame  (new frame% [label "Circle Drawer"][width 400]))
(define hpane1 (new horizontal-pane% [parent frame][min-height 20][alignment '(center center)]))
(new button% [label "Undo"][parent hpane1][callback (λ _ (undo) (send canvas on-paint))])
(new button% [label "Redo"][parent hpane1][callback (λ _ (redo) (send canvas on-paint))])
(define hpane2 (new horizontal-panel% [parent frame][min-height 400][alignment '(center center)]))
(define canvas : (Instance Circle-Canvas%) (new circle-canvas% [parent hpane2][style '(border)]))

(send frame show #t)
