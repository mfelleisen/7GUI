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

(require 7GUI/Typed/sub-canvas)

(Sub-Canvas% Circle-Canvas% circle)

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

(require 7GUI/Typed/sub-frame)
(sub-Frame Adjuster-Dialog% circle)

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
