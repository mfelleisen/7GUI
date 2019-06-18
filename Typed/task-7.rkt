#! /usr/bin/env gracket
#lang at-exp typed/racket/gui

;; a simple spreadsheet (will not check for circularities)

(require 7GUI/Typed/task-7-exp)
(require 7GUI/Typed/task-7-view)
(require 7GUI/Typed/double-click-canvas)

;; ---------------------------------------------------------------------------------------------------
(struct formula ({formula : Exp} {dependents : [Setof Ref]}) #:transparent)

(: *content Content)
(define *content  (make-immutable-hash)) ;; [Hashof Ref* Integer]
(: *formulas (Immutable-HashTable Ref formula))
(define *formulas (make-immutable-hash)) ;; [HashOF Ref* Formula] 

(define-syntax-rule (define-getr name : ResultType HashType (*source selector default))
  (define (name {ref : Ref}) : ResultType (selector (hash-ref *source ref (λ () default)))))

(define formula0 (formula 0 (set)))
(define-getr get-exp*       : Exp         formula (*formulas formula-formula formula0))
(define-getr get-dependents : [Setof Ref] formula (*formulas formula-dependents formula0))
(define-getr get-content    : Integer     Integer (*content values 0))

(: set-content! (-> Ref Integer Void))
(define (set-content! ref* vc)
  (define current (get-content ref*))
  (set! *content (hash-set *content ref* vc))
  (when (and current (not (= current vc)))
    (define f (get-dependents ref*))
    (when f (propagate-to f))))

(: propagate-to (-> [Setof Ref] Void))
(define (propagate-to dependents)
  (for ((d : Ref dependents))
    (set-content! d (evaluate (get-exp* d) *content))))
      
(: set-formula! (-> Ref Exp Void))
(define (set-formula! ref* exp*)
  (define new (formula exp* (or (get-dependents ref*) (set))))
  (set! *formulas (hash-set *formulas ref* new))
  (register-with-dependents (depends-on exp*) ref*)
  (set-content! ref* (evaluate exp* *content)))

(: register-with-dependents (-> [Setof Ref] Ref Void))
(define (register-with-dependents dependents ref*)
  (for ((d : Ref (in-set dependents)))
    (set! *formulas (hash-set *formulas d (formula (get-exp* d) (set-add (get-dependents d) ref*))))))

;; ---------------------------------------------------------------------------------------------------
(define cells-canvas% 
  (class canvas-double-click%
    (define/augment #;-final (on-click {x : Natural} {y : Natural}) (content-edit x y))
    (define/augment #;-final (on-double-click {x : Natural} {y : Natural}) (formula-edit x y))
    (super-new [paint-callback (lambda (_self dc) (paint-grid dc *content))])))

;; ---------------------------------------------------------------------------------------------------
;; cells and contents
(define-type Edit% (-> Natural Natural Void))

(: mk-edit (All (X Y) (-> String (-> String (U False X)) (-> Ref X Void) (-> Ref Y) Edit%)))
(define ((mk-edit title-fmt validator registration source) x y)
  (define ref (xy->A0 x y))
  (when (and (first ref) (second ref))
    (define value0 (~a (or (source ref) "")))
    (define dialog (new dialog% [style '(close-button)] [label (format title-fmt ref)]))
    (new text-field% [parent dialog] [label #f] [min-width 200] [min-height 80] [init-value value0]
         [callback (λ (self evt)
                     (when (eq? (send evt get-event-type) 'text-field-enter)
                       (define valid (validator (send self get-value)))
                       (when valid 
                         (registration ref valid)
                         (send dialog show #f))))])
    (send dialog show #t)))
      
(define formula-fmt "a formula for cell ~a")
(: formula-edit Edit%)
(define formula-edit (mk-edit formula-fmt string->exp* set-formula! (compose exp*->string get-exp*)))

(define content-fmt "content for cell ~a")
(define content-edit : Edit% (mk-edit content-fmt valid-content set-content! get-content))

;; ---------------------------------------------------------------------------------------------------
(define frame  (new frame% [label "Cells"][width (quotient WIDTH 2)][height (quotient HEIGHT 3)]))
(define canvas (new cells-canvas% [parent frame] [style '(hscroll vscroll)]))
(send canvas init-auto-scrollbars WIDTH HEIGHT 0. 0.)
(send canvas show-scrollbars #t #t)

(send frame show #t)
