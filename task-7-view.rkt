#lang racket/gui

(provide
 WIDTH
 HEIGHT 
 
 ;; DC [Hashof Ref Number] -> Void 
 paint-grid

 ;; Natural -> LETTERS 
 x->A

 ;; Natural -> INDEX
 y->0)

;; ---------------------------------------------------------------------------------------------------
(require 7GUI/task-7-exp)

(define HSIZE 100)
(define VSIZE 30)

(define X-OFFSET 2)
(define Y-OFFSET 10)

(define WIDTH  (* (+ (string-length LETTERS) 1) HSIZE))
(define HEIGHT (* 101 VSIZE))

(define (A->x letter)
  (for/first ((l (in-string LETTERS)) (i (in-naturals)) #:when (equal? l letter))
    (+ (* (+ i 1) HSIZE) X-OFFSET)))

(define (0->y index)
  (+ (* (+ index 1) VSIZE) Y-OFFSET))

(define ((finder range SIZE) x0)
  (define x (- x0 SIZE))
  (and (positive? x)
       (for/first ((r range) (i (in-naturals)) #:when (<= (+ (* i SIZE)) x (+ (* (+ i 1) SIZE)))) r)))

(define x->A (finder (in-string LETTERS) HSIZE))

(define y->0 (finder (in-range 100) VSIZE))

(define (paint-grid dc content)
  (send dc clear)
  (paint-hint  dc)
  (paint-axes  dc)
  (paint-cells dc content))

(define (paint-hint dc)
  (let* ([current-font (send dc get-font)])
    (send dc set-font small-font)
    (send dc draw-text "click for content" X-OFFSET 2)
    (send dc draw-text "double for formula" X-OFFSET 15)
    (send dc set-font current-font)))

(define (paint-axes dc)
  (send dc set-brush solid-gray)
  (for ((letter (in-string LETTERS)) (i (in-naturals)))
    (define x (* (+ i 1) HSIZE))
    (send dc draw-rectangle x 0 HSIZE VSIZE)
    (send dc draw-line x 0 x  HEIGHT)
    (send dc draw-text (string letter) (A->x letter) Y-OFFSET))
  (for ((i (in-range 100)))
    (define y (* (+ i 1) VSIZE))
    (send dc draw-line 0 y WIDTH y)
    (send dc draw-text (~a i) X-OFFSET (0->y i))))

(define (paint-cells dc content)
  (for (((key value) (in-hash content)))
    (match-define (list letter index) key)
    (define x0 (A->x letter))
    (define y0 (0->y index))
    (send dc draw-text (~a value) x0 y0)))

(define small-font (make-object font% 12 'roman))
(define solid-gray (new brush% [color "lightgray"]))
