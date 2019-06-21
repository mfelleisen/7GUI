#! /usr/bin/env gracket
#lang racket/gui

;; a simple spreadsheet (will not check for circularities)

(require 7GUI/should-be-racket)
(require 7GUI/task-7-exp)
(require 7GUI/task-7-view)
(require 7GUI/canvas-double-click)
(require 7GUI/Macros/7guis 7GUI/Macros/7state)

;; ---------------------------------------------------------------------------------------------------
(struct formula (formula dependents) #:transparent)
#; {Formula    =  [formula Exp* || Number || (Setof Ref*)]}

(define-syntax-rule (iff selector e default) (let ([v e]) (if v (selector v) default)))
(define (get-exp ref*) (iff formula-formula (hash-ref *formulas ref* #f) 0))
(define (get-dep ref*) (iff formula-dependents (hash-ref *formulas ref* #f) (set)))
(define (get-content ref*) (hash-ref *content ref* 0))

(define (set-content! ref* vc)
  (when (and* (get-content ref*) => (lambda (current) (not (= current vc))))
    (set! *content (values (hash-set *content ref* vc) ref*))))

(define (propagate-content-change _ ref*)
  (for ((d (in-set (get-dep ref*))))
    (set-content! d (evaluate (get-exp d) *content))))

(define-state *content (make-immutable-hash) propagate-content-change) ;; [Hashof Ref* Integer]

(define (set-formula! ref* exp*)
  (define new (formula exp* (get-dep ref*)))
  (set! *formulas (values (hash-set *formulas ref* new) ref* (depends-on exp*)))
  (set-content! ref* (evaluate exp* *content)))

(define (propagate-change-to-formulas _ ref dependents)
  (for ((d (in-set dependents)))
    (set! *formulas (stop (hash-set *formulas d (formula (get-exp d) (set-add (get-dep d) ref)))))))

(define-state *formulas (make-immutable-hash) propagate-change-to-formulas) ;; [HashOF Ref* Formula] 

;; ---------------------------------------------------------------------------------------------------
(define ccanvas%
  (class canvas-double-click%
    (define/augment-final (on-click x y) (content-edit x y))
    (define/augment-final (on-double-click x y) (formula-edit x y))
    (super-new [paint-callback (lambda (_self dc) (paint-grid dc *content))])))

;; ---------------------------------------------------------------------------------------------------
;; cells and contents 
(define ((mk-edit title-fmt validator setter source) x y)
  (define cell (list (x->A x) (y->0 y)))
  (when (and (first cell) (second cell))
    (define value0 (~a (or (source cell) "")))
    (gui #:id D #:frame (class dialog% (super-new [style '(close-button)])) (format title-fmt cell)
         (text-field% [label #f] [min-width 200] [min-height 80] [init-value value0]
                      [callback (λ (self evt)
                                  (when (eq? (send evt get-event-type) 'text-field-enter)
                                    (when*  (validator (send self get-value))
				      => (lambda (valid) (setter cell valid) (send D show #f)))))]))))
      
(define content-edit (mk-edit "content for cell ~a" valid-content set-content! get-content))

(define formula-fmt "a formula for cell ~a")
(define formula-edit (mk-edit formula-fmt string->exp* set-formula! (compose exp*->string get-exp)))

;; ---------------------------------------------------------------------------------------------------
(define-gui frame "Cells"
  (#:id canvas ccanvas% (min-width (/ WIDTH 2)) (min-height (/ HEIGHT 3)) [style '(hscroll vscroll)]))
(send canvas init-auto-scrollbars WIDTH HEIGHT 0. 0.)
(send canvas show-scrollbars #t #t)

(send frame show #t)
