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

(define-syntax-rule (iff selector e default) (let ([v e]) (if v (selector v) default)))
(define (get-exp* ref*) (iff formula-formula (hash-ref *formulas ref* #f) 0))
(define (get-dependents ref*) (iff formula-dependents (hash-ref *formulas ref* #f) (set)))
(define (get-content ref*) (hash-ref *content ref* 0))

(define (set-content! ref* vc)
  (define current (get-content ref*))
  (set! *content (hash-set *content ref* vc))
  (when (and current (not (= current vc)))
    (define f (get-dependents ref*))
    (when f (propagate-to f))))

(define (propagate-to dependents)
  (for ((d (in-set dependents)))
    (set-content! d (evaluate (get-exp* d) *content))))
      
(define (set-formula! ref* exp*)
  (define current (get-dependents ref*))
  (define new     (formula exp* (or current (set))))
  (set! *formulas (hash-set *formulas ref* new))
  (register-with-dependents (depends-on exp*) ref*)
  (set-content! ref* (evaluate exp* *content)))

(define (register-with-dependents dependents ref*)
  (for ((d (in-set dependents)))
    (define current (hash-ref *formulas d #f))
    (match-define (formula f old) (or current (formula 0 (set))))
    (set! *formulas (hash-set *formulas d (formula f (set-add old ref*))))))

;; ---------------------------------------------------------------------------------------------------
(define cells-canvas%
  (class canvas-double-click%
    (define/augment-final (on-click x y) (content-edit x y))
    (define/augment-final (on-double-click x y) (formula-edit x y))
    (super-new [paint-callback (lambda (_self dc) (paint-grid dc *content))])))

;; ---------------------------------------------------------------------------------------------------
;; cells and contents 
(define ((mk-edit title-fmt validator registration source) x y)
  (define cell (list (x->A x) (y->0 y)))
  (when (and (first cell) (second cell))
    (define value0 (~a (or (source cell) "")))
    (define dialog (new dialog% [style '(close-button)] [label (format title-fmt cell)]))
    (new text-field% [parent dialog] [label #f] [min-width 200] [min-height 80] [init-value value0]
         [callback (λ (self evt)
                     (when (eq? (send evt get-event-type) 'text-field-enter)
                       (define valid (validator (send self get-value)))
                       (when valid 
                         (registration cell valid)
                         (send dialog show #f))))])
    (send dialog show #t)))
      
(define content-edit (mk-edit "content for cell ~a" valid-content set-content! get-content))

(define formula-fmt "a formula for cell ~a")
(define formula-edit (mk-edit formula-fmt string->exp* set-formula! (compose exp*->string get-exp*)))

;; ---------------------------------------------------------------------------------------------------
(define frame  (new frame% [label "Cells"][width (/ WIDTH 2)][height (/ HEIGHT 3)]))
(define canvas (new cells-canvas% [parent frame] [style '(hscroll vscroll)]))
(send canvas init-auto-scrollbars WIDTH HEIGHT 0. 0.)
(send canvas show-scrollbars #t #t)

(send frame show #t)