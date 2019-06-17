#! /usr/bin/env gracket
#lang at-exp racket/gui

;; a simple spreadsheet (will not check for circularities)
;; -- This contains a bug that I discovered while I injected Macros. I should backport the fix. 

(require 7GUI/task-7-exp)
(require 7GUI/task-7-view)
(require 7GUI/canvas-double-click)

;; -----------------------------------------------------------------------------
(define (valid-content x)
  (define n (string->number x))
  (and n (integer? n) n))

(struct formula (formula dependents) #:transparent)
#; {Formula    =  [formula Exp* || Number || (Setof Ref*)]}

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
    (set-content! (first d) (second d) (evaluate exp* *content))))
      
(define (set-formula! letter index exp*)
  (define ref*    (list letter index))
  (define current (get-dependents letter index))
  (define new     (formula exp* (or current (set))))
  (set! *formulas (hash-set *formulas ref* new))
  (register-with-dependents (depends-on exp*) ref*)
  (set-content! letter index (evaluate exp* *content)))

(define (register-with-dependents dependents ref*)
  (for ((d (in-set dependents)))
    (define current (hash-ref *formulas d #f))
    (match-define (formula f old) (or current (formula 0 (set))))
    (set! *formulas (hash-set *formulas d (formula f (set-add old ref*))))))

;; ---------------------------------------------------------------------------------------------------
(define cells-canvas%
  (class canvas-double-click%
    (define/augment-final (on-click x y) (popup-content-editor x y))
    (define/augment-final (on-double-click x y) (popup-formula-editor x y))
    (super-new [paint-callback (lambda (_self dc) (paint-grid dc *content))])))

;; ---------------------------------------------------------------------------------------------------
;; cells and contents 
(define ((popup-editor title-fmt validator registration source) x y)
  (define letter (x->A x))
  (define index  (y->0 y))
  (when (and letter index)
    (define value0 (~a (or (source letter index) "")))
    (define dialog (new dialog% [style '(close-button)] [label (format title-fmt letter index)]))
    (new text-field% [parent dialog] [label #f] [min-width 200] [min-height 80] [init-value value0]
         [callback (λ (self evt)
                     (when (eq? (send evt get-event-type) 'text-field-enter)
                       (define valid (validator (send self get-value)))
                       (when valid 
                         (registration letter index valid)
                         (send dialog show #f))))])
    (send dialog show #t)))
      
(define popup-formula-editor
  (popup-editor "a formula for cell ~a~a" string->exp* set-formula! (compose exp*->string get-exp*)))

(define popup-content-editor
  (popup-editor "content for cell ~a~a" valid-content set-content! get-content))

;; ---------------------------------------------------------------------------------------------------
(define frame  (new frame% [label "Cells"][width (/ WIDTH 2)][height (/ HEIGHT 3)]))
(define canvas (new cells-canvas% [parent frame] [style '(hscroll vscroll)]))
(send canvas init-auto-scrollbars WIDTH HEIGHT 0. 0.)
(send canvas show-scrollbars #t #t)

(send frame show #t)