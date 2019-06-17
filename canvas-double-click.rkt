#lang racket/gui

(provide
 ;; a class extension for canvas$ that deals with single- and double-clicks
 ;; augment on-click and on-double-click to add the respective functionality 
 ;; each calls on-paint afterwards
 
 canvas-double-click%)

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

    ;; catching 
    (define *possible-double-click? #f)
    (define *x 0)
    (define *y 0)

    (define (timer-cb)
      (when *possible-double-click? (on-click *x *y))
      (set! *possible-double-click? #f))
    (define timer (new timer% [notify-callback timer-cb]))

    (define/overment (on-event evt)
      (when (eq? (send evt get-event-type) 'left-down)
        (set! *x (send evt get-x))
        (set! *y (send evt get-y))
        (cond
          [(not *possible-double-click?)
           (set! *possible-double-click? #t)
           ;; #true so that it doesn't get re-started 
           (send timer start DOUBLE-CLICK-INTERVAL #true)]
          [else
           (send timer stop)
           (set! *possible-double-click? #f)
           (on-double-click *x *y)])))
    
    (super-new)))

;; -----------------------------------------------------------------------------

