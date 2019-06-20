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
    
    (define-state *possible-double-click? #f
      (λ (pdc) (if pdc (send timer start DOUBLE-CLICK-INTERVAL) (send timer stop))))
    
    (define *x 0)
    (define *y 0)

    (define (timer-cb)
      (when *possible-double-click? (on-click *x *y))
      (set! *possible-double-click? #f))
    (define timer (new timer% [notify-callback timer-cb]))

    (define/overment (on-event evt)
      (cond
        [(eq? (send evt get-event-type) 'left-down)
         (set!-values (*x *y) (values (send evt get-x) (send evt get-y)))
         (cond
           [(not *possible-double-click?) (set! *possible-double-click? #t)]
           [else (set! *possible-double-click? #f) (on-double-click *x *y)])]
        [else (inner (void) on-event evt)]))
    
    (super-new)))
