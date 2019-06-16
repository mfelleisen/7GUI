#! /usr/bin/env gracket
#lang at-exp racket/gui

;; a simple spreadsheet (will not check for circularities)

(require 7GUI/Macros/7guis)
(require 7GUI/task-7-exp)

;; ---------------------------------------------------------------------------------------------------
(define (valid-content x)
  (define n (string->number x))
  (and n (integer? n) n))

(struct formula (formula dependents) #:transparent)
#; {Formula    =  [formula Exp* || Number || (Setof Ref*)]}

(define-syntax-rule (iff selector e default) (let ([v e]) (if v (selector v) default)))
  
(define (get-exp* L I) (iff formula-formula (hash-ref *formulas (list L I) #f) 0))
(define (get-dependents L I) (iff formula-dependents (hash-ref *formulas (list L I) #f) (set)))
(define (get-content L I) (hash-ref *content (list L I) 0))

(define (set-content! letter index vc)
  (define current (get-content letter index))
  (when (and current (not (= current vc)))
    (set! *content (many (list (hash-set *content (list letter index) vc) letter index current vc)))))

(define (propagate-content-change new-hash letter index current vc)
  (define dependents (get-dependents letter index))
  (for ((d (in-set dependents)))
    (define exp* (get-exp* (first d) (second d)))
    (set-content! (first d) (second d) (evaluate exp* *content))))

(define-state *content (make-immutable-hash) propagate-content-change) ;; [Hashof Ref* Integer]

(define (set-formula! letter index exp*)
  (define new (formula exp* (get-dependents letter index)))
  (define ref (list letter index))
  (set! *formulas (many (list (hash-set *formulas ref new) ref (depends-on exp*))))
  (set-content! letter index (evaluate exp* *content)))

(define (propagate-change-to-formulas _ ref dependents)
  (for ((d (in-set dependents)))
    (match-define (list d-let d-ind) d)
    (define new-deps (set-add (get-dependents d-let d-ind) ref))
    (set! *formulas (stop (hash-set *formulas d (formula (get-exp* d-let d-ind) new-deps))))))

(define-state *formulas (make-immutable-hash) propagate-change-to-formulas) ;; [HashOF Ref* Formula] 

;; ---------------------------------------------------------------------------------------------------
(define DOUBLE-CLICK-INTERVAL (send (new keymap%) get-double-click-interval))

(define cells-canvas%
  (class canvas%
    (define *double? #f)
    (define *x 0)
    (define-state *y 0 (λ _ (set! *double? (not *double?))))
    
    (define (timer-cb)
      (when *double?
        (content-edit *x *y)
        (paint-callback this (send this get-dc)))
      (set! *double? #f))
    (define timer (new timer% [notify-callback timer-cb]))
    
    (define/override (on-event evt)
      (when (eq? (send evt get-event-type) 'left-down)
        (set!-values (*x *y) (values (send evt get-x) (send evt get-y)))
        (if *double?
 	    ;; #true so that it doesn't get re-started 
            (send timer start DOUBLE-CLICK-INTERVAL #true)
            (begin (send timer stop) (set! *double? #f) (formula-edit *x *y) (send this on-paint)))))
    
    (define (paint-callback _self dc) (paint-grid dc *content))
    
    (super-new [paint-callback paint-callback])))

;; ---------------------------------------------------------------------------------------------------
;; grid layout 
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
  
  (let* ([current-font (send dc get-font)])
    (send dc set-font small-font)
    (send dc draw-text "click for content" X-OFFSET 2)
    (send dc draw-text "double for formula" X-OFFSET 15)
    (send dc set-font current-font))

  (send dc set-brush solid-gray)
  (for ((letter (in-string LETTERS)) (i (in-naturals)))
    (define x (* (+ i 1) HSIZE))
    (send dc draw-rectangle x 0 HSIZE VSIZE)
    (send dc draw-line x 0 x  HEIGHT)
    (send dc draw-text (string letter) (A->x letter) Y-OFFSET))
  (for ((i (in-range 100)))
    (define y (* (+ i 1) VSIZE))
    (send dc draw-line 0 y WIDTH y)
    (send dc draw-text (~a i) X-OFFSET (0->y i)))

  (for (((key value) (in-hash content)))
    (match-define (list letter index) key)
    (define x0 (A->x letter))
    (define y0 (0->y index))
    (send dc draw-text (~a value) x0 y0)))
(define small-font (make-object font% 12 'roman))
(define solid-gray (new brush% [color "lightgray"]))

;; ---------------------------------------------------------------------------------------------------
;; cells and contents 
(define ((mk-edit title-fmt validator setter source) x y)
  (define letter (x->A x))
  (define index  (y->0 y))
  (when (and letter index)
    (define value0 (~a (or (source letter index) "")))
    (define a-dialog% (class dialog% (super-new [style '(close-button)])))
    (gui #:id dialog #:frame a-dialog% (format title-fmt letter index)
         (text-field% [label #f] [min-width 200] [min-height 80]
                      [init-value value0]
                      [callback (λ (self evt)
                                  (when (eq? (send evt get-event-type) 'text-field-enter)
                                    (define valid (validator (send self get-value)))
                                    (when valid 
                                      (setter letter index valid)
                                      (send dialog show #f))))]))))
      
(define content-edit (mk-edit "content for cell ~a~a" valid-content set-content! get-content))

(define formula-edit
  (mk-edit "a formula for cell ~a~a" string->exp* set-formula! (compose exp*->string get-exp*)))

;; ---------------------------------------------------------------------------------------------------
(define frame  (new frame% [label "Cells"] [width (/ WIDTH 2)][height (/ HEIGHT 3)]))
(define canvas (new cells-canvas% [parent frame] [style '(hscroll vscroll)]))
(send canvas init-auto-scrollbars WIDTH HEIGHT 0. 0.)
(send canvas show-scrollbars #t #t)

(send frame show #t)
