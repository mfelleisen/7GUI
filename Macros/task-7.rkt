#! /usr/bin/env gracket
#lang racket/gui

;; a simple spreadsheet (will not check for circularities)

(require 7GUI/task-7-exp)
(require 7GUI/task-7-view)
(require 7GUI/canvas-double-click)
(require 7GUI/Macros/7guis)

;; ---------------------------------------------------------------------------------------------------
(define (valid-content x)
  (define n (string->number x))
  (and n (integer? n) n))

(struct formula (formula dependents) #:transparent)
#; {Formula    =  [formula Exp* || Number || (Setof Ref*)]}

(define-syntax-rule (iff selector e default) (let ([v e]) (if v (selector v) default)))
(define (get-exp* ref*) (iff formula-formula (hash-ref *formulas ref* #f) 0))
(define (get-dependents ref*) (iff formula-dependents (hash-ref *formulas ref* #f) (set)))
(define (get-content ref*) (hash-ref *content ref* 0))

(define (set-content! ref* vc)
  (define current (get-content ref*))
  (when (and current (not (= current vc)))
    (set! *content (many (list (hash-set *content ref* vc) ref* current vc)))))

(define (propagate-content-change _ ref* current vc)
  (define dependents (get-dependents ref*))
  (for ((d (in-set dependents)))
    (set-content! d (evaluate (get-exp* d) *content))))

(define-state *content (make-immutable-hash) propagate-content-change) ;; [Hashof Ref* Integer]

(define (set-formula! ref* exp*)
  (define new (formula exp* (get-dependents ref*)))
  (set! *formulas (many (list (hash-set *formulas ref* new) ref* (depends-on exp*))))
  (set-content! ref* (evaluate exp* *content)))

(define (propagate-change-to-formulas _ ref dependents)
  (for ((d (in-set dependents)))
    (define new-deps (set-add (get-dependents d) ref))
    (set! *formulas (stop (hash-set *formulas d (formula (get-exp* d) new-deps))))))

(define-state *formulas (make-immutable-hash) propagate-change-to-formulas) ;; [HashOF Ref* Formula] 

;; ---------------------------------------------------------------------------------------------------
(define cells-canvas%
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
                                    (define valid (validator (send self get-value)))
                                    (when valid 
                                      (setter cell valid)
                                      (send D show #f))))]))))
      
(define content-edit (mk-edit "content for cell ~a" valid-content set-content! get-content))

(define formula-fmt "a formula for cell ~a")
(define formula-edit (mk-edit formula-fmt string->exp* set-formula! (compose exp*->string get-exp*)))

;; ---------------------------------------------------------------------------------------------------
(define-gui frame "Cells" (#:id canvas cells-canvas% [style '(hscroll vscroll)])) ;; TOO SMALL 
(send canvas init-auto-scrollbars WIDTH HEIGHT 0. 0.)
(send canvas show-scrollbars #t #t)

(send frame show #t)