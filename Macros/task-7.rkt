#! /usr/bin/env gracket
#lang at-exp racket/gui

;; a simple spreadsheet (will not check for circularities)

(require 7GUI/Macros/7guis)

;; ---------------------------------------------------------------------------------------------------
(define LETTERS  "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
#; {Index      : N in [0,99]}
#; {Reference  is a Letter followed by an Index}
#; {Expression =  Reference || Integer || (+ Expression Expression)}

(struct formula (formula dependents) #:transparent)
#; {Ref*       =  (List Letter Index)}
#; {Exp*       =  Ref* || (list '+ Exp* Exp*)}
#; {Formula    =  [formula Exp* || Number || (Setof Ref*)]}

(define (render-exp* exp*)
  (if (boolean? exp*)
      ""
      (let render-exp* ((exp* exp*))
        (match exp*
          [(? number?) (~a exp*)]
          [(list letter index) (~a letter index)]
          [(list '+ left right) (format "(+ ~a ~a)" (render-exp* left) (render-exp* right))]))))

(define *content  (make-immutable-hash)) ;; [Hashof Ref* Integer]
(define *formulas (make-immutable-hash)) ;; [HashOF Ref* Formula] 

(define-syntax-rule (define-getr name (*source selector))
  (define (name letter index)
    (define f (hash-ref *source (list letter index) #f))
    (and f (selector f))))

(define-getr get-exp* (*formulas formula-formula))
(define-getr get-dependents (*formulas formula-dependents))
(define-getr get-content (*content values))

(define (set-content! letter index vc)
  (define ref* (list letter index))
  (define current (get-content letter index))
  (set! *content (hash-set *content ref* vc))
  (when (and current (not (= current vc)))
    (define f (get-dependents letter index))
    (when f (propagate-to f))))

(define (propagate-to dependents)
  (for ((d dependents))
    (define exp* (get-exp* (first d) (second d)))
    (set-content! (first d) (second d) (evaluate-exp exp*))))
      
(define (set-formula! letter index exp*)
  (define ref*    (list letter index))
  (define current (get-dependents letter index))
  (define new     (formula exp* (or current (set))))
  (set! *formulas (hash-set *formulas ref* new))
  (register-with-dependents (dependents exp*) ref*)
  (set-content! letter index (evaluate-exp exp*)))

(define (register-with-dependents dependents ref*)
  (for ((d (in-set dependents)))
    (define current (hash-ref *formulas d #f))
    (match-define (formula f old) (or current (formula 0 (set))))
    (set! *formulas (hash-set *formulas d (formula f (set-add old ref*))))))

#; {Exp* -> (Listof Ref*)}
(define (dependents exp*)
  (let loop ([exp* exp*][accumulator (set)])
    (match exp*
      [(? number?) accumulator]
      [(list L I) (set-add accumulator exp*)]
      [(list '+ left right) (loop left (loop right accumulator))])))

#;{ Exp* -> Integer}
(define (evaluate-exp exp*)
  (let loop ([exp* exp*])
    (match exp*
      [(? number?) exp*]
      [(list L I) (get-content L I)]
      [(list '+ left right) (+ (loop left) (loop right))])))

(define (valid-formula x)
  (define ip (open-input-string x))
  (define y (read ip))
  (and (eof-object? (read ip))
       (let loop ([y y])
         (match y
           [(? valid-cell) (valid-cell y)]
           [(? integer?) y]
           [(list '+ y1 y2) (list '+ (loop y1) (loop y2))]
           [else #f]))))

(define (valid-cell x:sym)
  (and (symbol? x:sym)
       (let* ([x:str (symbol->string x:sym)]
              [x (regexp-match #px"([A-Z])(\\d\\d)" x:str)])
         (or (and x (split x))
             (let ([x (regexp-match #px"([A-Z])(\\d)" x:str)])
               (and x (split x)))))))

(define (split x)
  (match x [(list _ letter index) (list (string-ref letter 0) (string->number index))]))

(define (valid-content x)
  (define n (string->number x))
  (and n (integer? n) n))

;; ---------------------------------------------------------------------------------------------------
(define DOUBLE-CLICK-INTERVAL (send (new keymap%) get-double-click-interval))

(define cells-canvas
  (class canvas%
    (inherit on-paint get-dc)

    (define *possible-double-click? #f)
    (define *x 0)
    (define *y 0)

    (define (timer-cb)
      (when *possible-double-click?
        (popup-content-editor *x *y)
        (paint-callback this 'y))
      (set! *possible-double-click? #f))
    (define timer (new timer% [notify-callback timer-cb]))
    
    (define/override (on-event evt)
      (when (eq? (send evt get-event-type) 'left-down)
        (set! *x (send evt get-x))
        (set! *y (send evt get-y))
        (cond
          [(not *possible-double-click?)
           (set! *possible-double-click? #t)
           (send timer start DOUBLE-CLICK-INTERVAL)]
          [else
           (send timer stop)
           (set! *possible-double-click? #f)
           (popup-formula-editor *x *y)
           (paint-callback this 'y)])))
    
    (define (paint-callback _self _evt) (paint-grid (get-dc) *content))
    
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
(define ((popup-editor title-fmt validator registration source) x y)
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
                                      (registration letter index valid)
                                      (send dialog show #f))))]))))
      
(define popup-formula-editor
  (popup-editor "a formula for cell ~a~a" valid-formula set-formula! (compose render-exp* get-exp*)))

(define popup-content-editor
  (popup-editor "content for cell ~a~a" valid-content set-content! get-content))

;; ---------------------------------------------------------------------------------------------------
(define frame  (new frame% [label "Cells"] [width (/ WIDTH 2)][height (/ HEIGHT 3)]))
(define canvas (new cells-canvas [parent frame] [style '(hscroll vscroll)]))
(send canvas init-auto-scrollbars WIDTH HEIGHT 0. 0.)
(send canvas show-scrollbars #t #t)

(send frame show #t)