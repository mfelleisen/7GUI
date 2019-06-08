#! /usr/bin/env gracket
#lang at-exp racket/gui

;; a circle drawer with undo/redo facilities (unclear spec for resizing)

;; ---------------------------------------------------------------------------------------------------
(require 7GUI/Macros/7guis)

;; ---------------------------------------------------------------------------------------------------
(define Default-Diameter 20)

(struct circle (x y d action) #:transparent)

(define (draw-1-circle dc brush c)
  (send dc set-brush brush)
  (match-define (circle x y d _a) c)
  (send dc draw-ellipse x y d d))

(define-state *circles '() (lambda (x) (send canvas on-paint)))

(define-state *history '() (lambda (x) (send canvas on-paint)))

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
  (argmin (distance xm ym) circles))

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
    (define *in-adjuster #f) ;; we can get a quasi-modal dialog this way 
    (define/public (unlock) (set! *in-adjuster #f) (displayln 'unlocked))
    (define/private (lock) (set! *in-adjuster #t) (displayln 'locked))

    (define-state *x 0 (λ (x) (send this on-paint)))
    (define-state *y 0 values)

    (define/override (on-event evt)
      (unless *in-adjuster
        (define type (send evt get-event-type))
        (set! *x (send evt get-x))
        (set! *y (send evt get-y))
        (case type
          [(leave) (set! *x #f)]
          [(enter) (set! *x 0)]
          [(left-down)  (when (is-empty-area *x *y) (add-circle! *x *y))]
          [(right-down) (when (cons? *circles) (lock) (popup-adjuster this (the-closest *x *y)))])))
    
    (define (paint-callback _self _evt)
      (cond
        [(empty? *circles) (send (send this get-dc) clear)]
        [(boolean? *x)     (draw-circles #f)]
        [else              (draw-circles (the-closest *x *y))]))
    
    (define/public (draw-circles closest (others-without-closest #f))
      (send dc clear)
      (for ((c (or others-without-closest *circles))) (draw-1-circle dc white-brush c))
      (when closest (draw-1-circle dc solid-gray closest)))
    
    (super-new [paint-callback paint-callback])

    (define dc (send this get-dc))))

(define (popup-adjuster canvas closest-circle)
  (define (cb _ evt)(when (eq? (send evt get-event-type) 'menu-popdown-none) (send canvas unlock)))
  (define pm (new popup-menu% [title "adjuster"][popdown-callback cb]))
  (new menu-item% [parent pm] [label "adjust radius"] [callback (adjuster! canvas closest-circle)])
  (send frame popup-menu pm  100 100))

(define ((adjuster! canvas closest-circle) . x)
  (match-define (circle x0 y0 d0 _a) closest-circle)
  (define others (remq closest-circle *circles))
  (define-state *d d0 (λ (d) (send canvas draw-circles (circle x0 y0 d '_ephemeral_) others)))
  
  (define adjuster-dialog%
    (class frame%
      (define/augment (on-close) (send canvas unlock) (resize! closest-circle *d))
      (super-new)))

  (define a-slider% (class slider% (super-new [label ""] [min-value 10] [max-value 100])))
  
  (gui #:frame adjuster-dialog% (format "Adjust radius of circle at (~a,~a)" x0 y0)
       (#:id s a-slider% #:change *d (λ (_) (send s get-value)) [init-value d0])))

;; ---------------------------------------------------------------------------------------------------
(define-gui frame "Circle Drawer"
  (#:horizontal
   (button% [label "Undo"][callback (λ _ (undo))]) (button% [label "Redo"][callback (λ _ (redo))]))
  (#:id canvas circle-canvas% [min-height 400][min-width 400][style '(border)]))

(send frame show #t)