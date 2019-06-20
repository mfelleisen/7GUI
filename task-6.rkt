#! /usr/bin/env gracket
#lang at-exp racket/gui

;; a circle drawer with undo/redo facilities (unclear spec for resizing)

;; ---------------------------------------------------------------------------------------------------
(define Default-Diameter 20)

(struct circle (x y d action) #:transparent)

(define (draw-1-circle dc brush c)
  (match-define (circle x y d _a) c)
  (send dc set-brush brush)
  (define r (/ d 2))
  (send dc draw-ellipse (- x r) (- y r) d d))

(define *circles '())

(define *history '())

(define (add-circle! x y)
  (define added (circle x y Default-Diameter 'added))
  (set! *circles (cons added *circles)))
    
(define (resize! old-closest new-d)
  (match-define (circle x y d a) old-closest)
  (define resized
    (match a
      ['added (circle x y new-d `(resized (,d)))]
      [`(resized . ,old-sizes) (circle x y new-d `(resized ,(cons d old-sizes)))]))
  (set! *circles (cons resized (remq old-closest *circles))))

(define (undo)
  (when (cons? *circles)
    (define fst (first *circles))
    (match fst
      [(circle x y d 'added) (set! *circles (rest *circles))]
      [(circle x y d `(resized (,r0 . ,sizes)))
       (set! *circles (cons (circle x y r0 `(resized (,d))) (rest *circles)))])
    (set! *history (cons fst *history))))

(define (redo)
  (when (cons? *history)
    (define fst (first *history))
    (if (eq? (circle-action fst) 'added)
        (set!-values (*circles *history) (values (cons fst *circles) (rest *history)))
        (set!-values (*circles *history) (values (cons fst (rest *circles)) (rest *history))))))

(define (the-closest xm ym (circles *circles))
  (define cdistance (distance xm ym))
  (define-values (good-circles distance*)
    (for*/fold ([good-circles '()][distance* '()])
               ((c circles) (d (in-value (cdistance c))) #:when (< d (/ (circle-d c) 2)))
      (values (cons c good-circles) (cons d distance*))))
  (and (cons? distance*) (first (argmin second (map list good-circles distance*)))))

(define (is-empty-area xm ym (circles *circles))
  (define dist (distance xm ym))
  (for/and ((c circles)) (> (dist c) (/ (+ (circle-d c) Default-Diameter) 2))))

;; N N (Circle -> Real]
(define ((distance xm ym) c)
  (match-define (circle xc yc _d _a) c)
  (sqrt (+ (expt (- xc xm) 2) (expt (- yc ym) 2))))

;; ---------------------------------------------------------------------------------------------------
(define solid-gray  (new brush% [color "gray"]))
(define white-brush (new brush% [color "white"]))

(define circle-canvas%
  (class canvas%
    (inherit on-paint get-dc)
    
    (define *in-adjuster #f) ;; we can get a quasi-modal dialog this way 
    (define/public (unlock) (set! *in-adjuster #f))
    (define/private (lock) (set! *in-adjuster #t))

    (define *x 0)
    (define *y 0)

    (define/override (on-event evt)
      (unless *in-adjuster
        (define type (send evt get-event-type))
        (set! *x (send evt get-x))
        (set! *y (send evt get-y))
        (cond
          [(eq? 'leave type) (set! *x #f)]
          [(eq? 'enter type) (set! *x 0)]
          [(and (eq? 'left-down type) (is-empty-area *x *y)) (add-circle! *x *y)]
          [(and (eq? 'right-down type) (the-closest *x *y)) => (λ (tc) (lock) (popup-adjuster tc))])
        (paint-callback this 'y)))
    
    (define (paint-callback _self _evt)
      (cond
        [(empty? *circles) (send (send this get-dc) clear)]
        [(boolean? *x)     (draw-circles #f)]
        [else              (draw-circles (the-closest *x *y))]))
    
    (define/public (draw-circles closest (others-without-closest #f))
      (define dc (get-dc))
      (send dc clear)
      (for ((c (or others-without-closest *circles))) (draw-1-circle dc white-brush c))
      (when closest (draw-1-circle dc solid-gray closest)))
    
    (super-new [paint-callback paint-callback])))

(define (popup-adjuster closest-circle)
  (define (cb _ evt)(when (eq? (send evt get-event-type) 'menu-popdown-none) (send canvas unlock)))
  (define pm (new popup-menu% [title "adjuster"][popdown-callback cb]))
  (new menu-item% [parent pm] [label "adjust radius"] [callback (adjuster! closest-circle)])
  (send frame popup-menu pm  100 100))

(define ((adjuster! closest-circle) . x)
  (define d0 (circle-d closest-circle))
  (define frame (new adjuster-dialog% [closest-circle closest-circle]))
  (new adjuster-slider% [parent frame][init-value d0][update (λ (x) (send frame continuous x))])
  (send frame show #t))

(define adjuster-dialog%
  (class frame% (init-field closest-circle)
    (match-define (circle x* y* *d _a) closest-circle)
    (define others (remq closest-circle *circles))
    
    (define/public (continuous new-d) ;; resize locally while adjusting 
      (set! *d new-d)
      (send canvas draw-circles (circle x* y* *d '_dummy_) others))
    
    (define/augment (on-close) ;; resize globally 
      (send canvas unlock)
      (resize! closest-circle *d))

    (super-new [label (format "Adjust radius of circle at (~a,~a)" x* y*)])))

(define adjuster-slider%
  (class slider% (init-field update)
    (inherit get-value)
    (super-new [label ""][min-value 10][max-value 100][callback (λ _ (update (get-value)))])))

;; ---------------------------------------------------------------------------------------------------
(define frame  (new frame% [label "Circle Drawer"][width 400]))
(define hpane1 (new horizontal-pane% [parent frame][min-height 20][alignment '(center center)]))
(new button% [label "Undo"][parent hpane1][callback (λ _ (undo) (send canvas on-paint))])
(new button% [label "Redo"][parent hpane1][callback (λ _ (redo) (send canvas on-paint))])
(define hpane2 (new horizontal-panel% [parent frame][min-height 400][alignment '(center center)]))
(define canvas (new circle-canvas% [parent hpane2][style '(border)]))

(send frame show #t)
