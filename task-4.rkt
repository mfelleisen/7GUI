#lang at-exp racket/gui

;; a timer that permits the continuous setting of a new interval, plus
;; a gauge and a text field that display the fraction of the elapsed time
;; a reset button that sends the elapsed time back to 0

(define INTERVAL 100)

(define *elapsed  0) ;; INTERVAL/1000 ms accumulated elapsed time
(define *duration 0) ;; INTERVAL/1000 ms set duration interval 

(define (timer-cb)
  (unless (>= *elapsed *duration)
    (set! *elapsed (+ *elapsed 1))
    (send timer start INTERVAL)
    (elapsed-cb)))
(define timer (new timer% [notify-callback timer-cb]))

(define (reset-cb . x)
  (send timer stop)
  (set! *elapsed 0)
  (timer-cb))

(define (duration-cb self _evt)
  (define new-duration (send self get-value))
  (unless (= new-duration *duration)
    (send timer stop)
    (set! *duration new-duration)
    (timer-cb)))

(define (elapsed-cb)
  (send text set-value (format "elapsed ~a" *elapsed))
  (unless (zero? *duration)
    (define r (quotient (* 100 *elapsed) *duration))
    (send elapsed set-value r)))

(define frame   (new frame% [label "timer"]))
(define elapsed (new gauge% [label "elapsed"][parent frame][enabled #f][range 100]))
(define text    (new text-field% [parent frame][init-value "0"][label ""]))
(new slider% [label "duration"][parent frame][min-value 0][max-value 100][callback duration-cb])
(new button% [label "reset"][parent frame][callback reset-cb])

(elapsed-cb)
(send frame show #t)
