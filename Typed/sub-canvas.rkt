#lang at-exp typed/racket/gui

(provide define-type-canvas)

(define-syntax-rule (define-type-canvas name-of-type% extra ...)
  (define-type name-of-type%
    (Class #:implements Canvas%
           ;; new methods 
           extra ...

           ;; repeat init arguments
           @;{From Asumu:
              Duplicating the inits is unfortunate, but to avoid this there would need to be some kind
              of fine-grained syntax for, say, subtracting specific init args but keeping the rest. }

             (init (parent (Instance Area-Container<%>))
                   (style
                    (Listof
                     (U 'border
                        'combo
                        'control-border
                        'deleted
                        'gl
                        'hscroll
                        'no-autoclear
                        'no-focus
                        'resize-corner
                        'transparent
                        'vscroll))
                    #:optional)

                   ;; -----------------------------------------------------------------------------
                   @;{ It is not clear to me yet, why I have to comment out the following line.
                       It is the only superfield that I populate via supernew. It's optional. So?}
                     
                     (paint-callback (-> (Instance Canvas%) (Instance DC<%>) Any) #:optional)
                     ;; -----------------------------------------------------------------------------

                     (label (U False String) #:optional)
                     (gl-config Any #:optional)
                     (enabled Any #:optional)
                     (vert-margin Nonnegative-Integer #:optional)
                     (horiz-margin Nonnegative-Integer #:optional)
                     (min-width (U Exact-Nonnegative-Integer False) #:optional)
                     (min-height (U Exact-Nonnegative-Integer False) #:optional)
                     (stretchable-width Any #:optional)
                     (stretchable-height Any #:optional)))))
