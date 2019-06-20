#lang racket/gui

(provide
 ;; a class extension for canvas$ that deals with single- and double-clicks
 ;; augment on-click and on-double-click to add the respective functionality 
 ;; each calls on-paint afterwards
 
 canvas-double-click%)

(require 7GUI/Macros/7state) ;; it improves readability 

(define DOUBLE-CLICK-INTERVAL (send (new keymap%) get-double-click-interval))

(define canvas-double-click%
  (class canvas%

    (inherit on-paint)

    (define/pubment (on-click x y)
      (inner (void) on-click x y)
      (on-paint))
    
    (define/pubment (on-double-click x y)
      (inner (void) on-double-click x y)
      (on-paint))
    
    (define-state *single-click? #f
      (λ (pdc) (if pdc (send timer start DOUBLE-CLICK-INTERVAL) (send timer stop))))
    
    (define *evt 0)
    (define (call f) (f (send *evt get-x) (send *evt get-y)))

    (λ x (on-click . x))

    (define (timer-cb)
      (when *single-click? (call (λ x (on-click . x))))
      (set! *single-click? #f))
    (define timer (new timer% [notify-callback timer-cb]))

    (define/overment (on-event evt)
      (cond
        [(eq? (send evt get-event-type) 'left-down)
         (set! *evt evt)
         (set! *single-click? (not *single-click?))
         (unless *single-click? (call (λ x (on-double-click . x))))]
        [else (inner (void) on-event evt)]))
    
    (super-new)))
