#! /usr/bin/env gracket
#lang at-exp typed/racket/gui

;; a simple spreadsheet (will not check for circularities)
;; -- This contains a bug that I discovered while I injected Macros. I should backport the fix. 

(require 7GUI/Typed/task-7-exp)

;; -----------------------------------------------------------------------------
(: valid-content (-> String (U False Integer)))
(define (valid-content x)
  (define n (string->number x))
  (and n (if (and (integer? n) (exact? n)) n #f)))

(struct formula ({formula : Exp} {dependents : [Setof Ref]}) #:transparent)
#; {Formula    =  [formula Exp* || Number || (Setof Ref*)]}

(: *content Content)
(define *content  (make-immutable-hash)) ;; [Hashof Ref* Integer]
(: *formulas (Immutable-HashTable Ref formula))
(define *formulas (make-immutable-hash)) ;; [HashOF Ref* Formula] 

(define-syntax-rule (define-getr name : ResultType (*source (selector Base)))
  (begin
    (: name (->* (Letter Index) ([Immutable-HashTable Ref Base][-> Base ResultType]) (U False ResultType)))
    (define (name letter index (*source *source) (selector selector))
      (: f (U ResultType False))
      (define f (hash-ref *source (list letter index) #f))
      (and f (selector f)))))

(define-getr get-exp*       : Exp (*formulas (formula-formula formula)))
(define-getr get-dependents : [Setof Ref] (*formulas (formula-dependents formula)))
(define-getr get-content    : Integer (*content (values Integer)))

(: set-content! (-> Letter Index Integer Void))
(define (set-content! letter index vc)
  (define ref* (list letter index))
  (define current (get-content letter index))
  (set! *content (hash-set *content ref* vc))
  (when (and current (not (= current vc)))
    (define f (get-dependents letter index))
    (when f (propagate-to f))))

(: propagate-to (-> [Setof Ref] Void))
(define (propagate-to dependents)
  (for ((d : Ref dependents))
    (define exp* (get-exp* (first d) (second d)))
    (set-content! (first d) (second d) (evaluate exp* *content))))
      
(: set-formula! (-> Letter Index Exp Void))
(define (set-formula! letter index exp*)
  (define ref*    (list letter index))
  (define current (get-dependents letter index))
  (define new     (formula exp* (or current (set))))
  (set! *formulas (hash-set *formulas ref* new))
  (register-with-dependents (depends-on exp*) ref*)
  (set-content! letter index (evaluate exp* *content)))

(: register-with-dependents (-> [Setof Ref] Ref Void))
(define (register-with-dependents dependents ref*)
  (for ((d : Ref (in-set dependents)))
    (define current (hash-ref *formulas d #f))
    (match-define (formula f old) (or current (formula 0 (set))))
    (set! *formulas (hash-set *formulas d (formula f (set-add old ref*))))))

;; ---------------------------------------------------------------------------------------------------
(define DOUBLE-CLICK-INTERVAL (send (new keymap%) get-double-click-interval))

(require 7GUI/Typed/sub-canvas)

(define-canvas Cells-Canvas%)

(define cells-canvas : Cells-Canvas%
  (class canvas%
    (inherit on-paint get-dc)

    (: *possible-double-click? Boolean)
    (define *possible-double-click? #f)
    (: *x Integer)
    (define *x 0)
    (: *y Integer)
    (define *y 0)

    (: timer-cb (-> Void))
    (define (timer-cb) 
      (when *possible-double-click?
        (when (>= *x 0)
          (when (>= *y 0)
            (popup-content-editor (cast *x Natural) (cast *y Natural))
            (paint-callback this (send this get-dc)))))
      (set! *possible-double-click? #f))
    (: timer (Instance Timer%))
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
           (when (>= *x 0)
             (when (>= *y 0)
               (popup-formula-editor (cast *x Natural) (cast *y Natural))))
           (paint-callback this (send this get-dc))])))

    (: paint-callback (-> Any (Instance DC<%>) Void))
    (define (paint-callback _self dc) (paint-grid dc))
    
    (super-new [paint-callback paint-callback])))

;; ---------------------------------------------------------------------------------------------------
;; grid layout 
(define HSIZE : Natural 100)
(define VSIZE : Natural 30)

(define X-OFFSET : Natural 2)
(define Y-OFFSET : Natural 10)

(define WIDTH : Natural (* (+ (string-length LETTERS) 1) HSIZE))
(define HEIGHT : Natural (* 101 VSIZE))

(: A->x (-> Letter Natural))
(define (A->x letter)
  (define r
    (for/list : [Listof Natural] ((l : Char (in-string LETTERS))
                                  (i : Natural (in-naturals))
                                  #:when (equal? l letter))
      (+ (* (+ i 1) HSIZE) X-OFFSET)))
  (or (and r (first r)) (error 'A->x "impossible")))

(: 0->y (-> Natural Natural))
(define (0->y index)
  (+ (* (+ index 1) VSIZE) Y-OFFSET))

(: finder (All (X) (-> (Sequenceof X) Natural (-> Natural (U False X)))))
(define ((finder range SIZE) x0)
  (define x (- x0 SIZE))
  (and (positive? x)
       (let ([r (for/list : (Listof X)
                  ((r : X range)
                   (i : Natural (in-naturals))
                   #:when (<= (+ (* i SIZE)) x (+ (* (+ i 1) SIZE))))
                  r)])
         (or (and r (first r)) (error 'finder "impossible")))))

(define x->A (finder (in-string LETTERS) HSIZE))

(define y->0 (finder (in-range 100) VSIZE))

(: paint-grid (-> (Instance DC<%>) Void))
(define (paint-grid dc)
  (send dc clear)
  
  (let* ([current-font (send dc get-font)])
    (send dc set-font small-font)
    (send dc draw-text "click for content" X-OFFSET 2)
    (send dc draw-text "double for formula" X-OFFSET 15)
    (send dc set-font current-font))

  (send dc set-brush solid-gray)
  (for ((letter : Char (in-string LETTERS)) (i (in-naturals)))
    (define x (* (+ i 1) HSIZE))
    (send dc draw-rectangle x 0 HSIZE VSIZE)
    (send dc draw-line x 0 x  HEIGHT)
    (send dc draw-text (string letter) (A->x letter) Y-OFFSET))
  (for ((i : Natural (in-range 100)))
    (define y (* (+ i 1) VSIZE))
    (send dc draw-line 0 y WIDTH y)
    (send dc draw-text (~a i) X-OFFSET (0->y i)))

  (for ((({key : Ref} {value : Integer}) (in-hash *content)))
    (match-define (list letter index) key)
    (define x0 (A->x letter))
    (define y0 (0->y index))
    (send dc draw-text (~a value) x0 y0)))
(define small-font (make-object font% 12 'roman))
(define solid-gray (new brush% [color "lightgray"]))

;; ---------------------------------------------------------------------------------------------------
;; cells and contents
(: popup-editor
   (All (X) (-> String (-> String (U False X)) (-> Letter Index X Void) (-> Letter Index X)
                (-> Natural Natural Void))))
(define ((popup-editor title-fmt validator registration source) x y)
  (define letter (x->A x))
  (define index  (y->0 y))
  (when (and letter index)
    (define value0 (~a (or (source letter index) "")))
    (define dialog (new dialog% [style '(close-button)] [label (format title-fmt letter index)]))
    (: field (Instance Text-Field%))
    (define field  (new text-field% [parent dialog] [label #f] [min-width 200] [min-height 80]
                        [init-value value0]
                        [callback (λ (self evt)
                                    (when (eq? (send evt get-event-type) 'text-field-enter)
                                      (define valid (validator (send field get-value)))
                                      (when valid 
                                        (registration letter index valid)
                                        (send dialog show #f))))]))
    (send dialog show #t)))
      
(define popup-formula-editor
  ((inst popup-editor Exp)
   "a formula for cell ~a~a" string->exp* set-formula! (compose exp*->string get-exp*)))

(define popup-content-editor
  ((inst popup-editor Integer)
   "content for cell ~a~a" valid-content set-content! get-content))

;; ---------------------------------------------------------------------------------------------------
(define frame  (new frame% [label "Cells"][width (quotient WIDTH 2)][height (quotient HEIGHT 3)]))
(define canvas (new cells-canvas [parent frame] [style '(hscroll vscroll)]))
(send canvas init-auto-scrollbars WIDTH HEIGHT 0. 0.)
(send canvas show-scrollbars #t #t)

(send frame show #t)