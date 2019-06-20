#lang at-exp typed/racket/gui

(provide
 ;; SYNTAX
 #; (define-type-canvas TypeName% [#:minus-init (init-param:id)] method-spec ...)
 #; {method-spec = name+type || (augment . name+type)}
 ;; creates a sub-type Class specification for a canvas,
 ;; -- subtracting init-paameters as init-param ... from those in Canva%
 ;; -- adding method specifications method-spec ...

 define-type-canvas)

;; ---------------------------------------------------------------------------------------------------
(require (for-syntax syntax/parse))
(require (for-syntax racket/function))

;; ---------------------------------------------------------------------------------------------------
(define-syntax (define-type-canvas stx)
  (syntax-parse stx 
    [(_ name-of-type% (~optional (~seq #:minus-init (y:id ...))) extra ...)
     #:do [(define yy (map syntax-e (syntax->list #'(~? (y ...) ()))))

           (define y-not-in-labels
             (for*/first ((z yy) (yy (in-value z)) #:unless (memf (curry eq? yy) labels)) z))
           (when y-not-in-labels
             (define fmt (format "cannot subtract ~a from ~a" y-not-in-labels labels))
             (raise-syntax-error 'define-type-canvas fmt))
           
           (define (in l) (memf (curry eq? l) yy))
           (define inits-included (for/list ([l labels][i inits] #:unless (in l)) i))]
     
     #`(define-type name-of-type%
         (Class #:implements Canvas%
                ;; new methods 
                extra ...

                ;; repeat init arguments
                @;{From Asumu:
      Duplicating the inits is unfortunate, but to avoid this there would need to be some kind
      of fine-grained syntax for, say, subtracting specific init args but keeping the rest. }
                  (init #,@inits-included)))]))

(define-for-syntax inits
  (syntax->list
   #'((parent (Instance Area-Container<%>))
      (style Style* #:optional)
      (paint-callback (-> (Instance Canvas%) (Instance DC<%>) Any) #:optional)
      (label (U False String) #:optional)
      (gl-config Any #:optional)
      (enabled Any #:optional)
      (vert-margin Nonnegative-Integer #:optional)
      (horiz-margin Nonnegative-Integer #:optional)
      (min-width (U Exact-Nonnegative-Integer False) #:optional)
      (min-height (U Exact-Nonnegative-Integer False) #:optional)
      (stretchable-width Any #:optional)
      (stretchable-height Any #:optional))))

(define-type Style
  (U 'combo
     'border 'control-border 'gl 'hscroll 'vscroll 'resize-corner
     'deleted 'no-autoclear 'no-focus
     'transparent))

(define-type Style* (Listof Style))

(define-for-syntax labels
  (map (compose syntax-e car syntax-e) inits))

;; ---------------------------------------------------------------------------------------------------

;; - - - "tests" - - - 
(define-type-canvas Without% #:minus-init (paint-callback))
;; make sure that we don't misspell init parameters to be subtracted 
; (define-type-canvas X% #:minus-init (bad))
(define-type-canvas With%)
(define (pc {cc : (Instance Canvas<%>)} {dc : (Instance DC<%>)}) (void))
;; make sure we get the correct result type 
; (define c-with : With% (class canvas% (super-new [paint-callback pc])))
(define c-without : Without% (class canvas% (super-new [paint-callback pc])))
