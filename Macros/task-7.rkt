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
  
(define (get-exp* L I) (iff formula-formula (hash-ref *formulas (list L I) #f) 0))
(define (get-dependents L I) (iff formula-dependents (hash-ref *formulas (list L I) #f) (set)))
(define (get-content L I) (hash-ref *content (list L I) 0))

(define (set-content! letter index vc)
  (define current (get-content letter index))
  (when (and current (not (= current vc)))
    (set! *content (many (list (hash-set *content (list letter index) vc) letter index current vc)))))

(define (propagate-content-change _new-hash letter index current vc)
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
(define cells-canvas%
  (class canvas-double-click%
    (define/augment-final (on-click x y) (content-edit x y))
    (define/augment-final (on-double-click x y) (formula-edit x y))
    (super-new [paint-callback (lambda (_self dc) (paint-grid dc *content))])))

;; ---------------------------------------------------------------------------------------------------
;; cells and contents 
(define ((mk-edit title-fmt validator setter source) x y)
  (define L (x->A x))
  (define I  (y->0 y))
  (when (and L I)
    (define value0 (~a (or (source L I) "")))
    (gui #:id D #:frame (class dialog% (super-new [style '(close-button)])) (format title-fmt L I)
         (text-field% [label #f] [min-width 200] [min-height 80] [init-value value0]
                      [callback (λ (self evt)
                                  (when (eq? (send evt get-event-type) 'text-field-enter)
                                    (define valid (validator (send self get-value)))
                                    (when valid 
                                      (setter L I valid)
                                      (send D show #f))))]))))
      
(define content-edit (mk-edit "content for cell ~a~a" valid-content set-content! get-content))

(define formula-fmt "a formula for cell ~a~a")
(define formula-edit (mk-edit formula-fmt string->exp* set-formula! (compose exp*->string get-exp*)))

;; ---------------------------------------------------------------------------------------------------
(define-gui frame #:frame (class frame% (super-new [width (/ WIDTH 2)][height (/ HEIGHT 3)])) "Cells"
  (#:id canvas cells-canvas% [parent frame] [style '(hscroll vscroll)]))

(send canvas init-auto-scrollbars WIDTH HEIGHT 0. 0.)
(send canvas show-scrollbars #t #t)
(send frame show #t)