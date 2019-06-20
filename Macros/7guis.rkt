#lang racket/gui

(provide
 ;; SYNTAX
 #; {(define-gui name:id Title:expr [#:frame expr] Title:expr gui-spec)}
 ;; -- defines name to be a frame-based GUI according to the optional #:frame and gui-spec
 ;; 
 #; {gui-spec    == gui-element ... || (gui-element ...)}
 #; {gui-element == ([#:id x:id] w:expr [#:change x:id f:expr] [l:id l0:expr] ...)
                 || (#:horizontal gui-element ...)
                 || (#:vertical gui-element ...) }
 ;; an atomic gui-element creates a widget using (new w [l l0] ...);
 ;; if it comes with a #:id x, it is given the name x with the same scope as name
 ;; if it comes with a #:change x f, a change to the state of w triggers (set! x (f old-x this)) 
 ;; a horizontal | vertical specification creates a horizontal | vertical pane for the nested elements

 #; {(gui [#:id id] [#:frame expr] Title:expr (state:id state:expr propagate:expr) gui-spec)}
 ;; like define-gui, but immediately shows the constructed frame 

 define-gui

 gui

 ;; #:change comes with a bunch of samll auxiliaries to make it useful: 

 ;; SYNTAX
 #; (with [#:post p:expr] [#:widget w:id] [#:method m] e ...)
 ;; a function uses
 ;; -- method m [get-value] to extract a value
 ;; -- from a GUI widget w [this]
 ;; -- post-processing it with p [identity]

 with

 ;; (All (X) [X -> X] -> [ X Any -> X])
 ;; a wrapper for computing just the new value from the old one 
 just

 ;; 
 none)

;; ---------------------------------------------------------------------------------------------------
(require (for-syntax syntax/parse))

(begin-for-syntax
  
  (define-syntax-class init
    #:description "name and value binding"
    (pattern (x:id e:expr)))

  (define-syntax-class gui-element
    #:description "gui element specification"
    (pattern ((~optional (~seq #:id x:id)) w:expr (~optional (~seq #:change s:id f:expr)) i:init ...))
    (pattern (#:horizontal ge:gui-element ...))
    (pattern (#:vertical ge:gui-element ...)))

  (define-syntax-class gui-spec
    #:description "gui specification"
    (pattern (ge:gui-element ...))))

(define-syntax (gui stx)
  (syntax-parse stx
    [(_ (~optional (~seq #:id x:id)) (~optional (~seq #:frame f)) T Vs ...)
     #'(begin (define-gui (~? x F) (~? (~@ #:frame f) (~@)) T Vs ...) (send (~? x F) show #t))]))

(define-syntax (define-gui stx)
  (syntax-parse stx
    [(_ frame-name:id (~optional (~seq #:frame f%:expr)) T:expr Vs:gui-element ...)
     #'(begin
         (define frame-name (new (~? f% frame%) [label T] [width 200] [height 77]))
         (define pane (new vertical-pane% [parent frame-name]))
         (setup-visuals pane (Vs))
         ...)]

    [(_ frame-name:id (~optional (~seq #:frame f%:expr)) Title:expr visuals:gui-spec)
     #'(begin
         (define frame-name (new (~? f% frame%) [label Title] [width 200] [height 77]))
         (setup-visuals frame-name visuals))]))

(define-syntax-rule (setup-visuals container (gui-specs ...))
  (gui-element container (#:horizontal gui-specs ...)))

(define-syntax (gui-element stx)
  (syntax-parse stx
    [(_ p (#:horizontal b ...))
     #'(begin (define horizontal (make-horizontal p)) (gui-element horizontal b) ...)]
    [(_ p (#:vertical b ...))
     #'(begin (define vertical (make-vertical p)) (gui-element vertical b) ...)]
    [(_ p [(~optional (~seq #:id x:id)) w%:expr (~optional (~seq #:change s:id f:expr)) o:init ...])
     #`(begin
         (~? (~@ (define g f)) (~@))
         [define (~? x y) (new w% [parent p]
                               (~? (~@ [callback
                                        (λ (self evt)
                                          (define new (g s self))
                                          (unless (*none? new) (set! s new)))])
                                   (~@))
                               o ...)])]))

(define ((just f) old _self) (f old))

(define-syntax (with stx)
  (syntax-parse stx
    [(_ x:id
        (~optional (~seq #:post f:expr))
        (~optional (~seq #:widget ff:id))
        (~optional (~seq #:method m:id))
        e ...)
     #:with self (datum->syntax stx 'self)
     #`(let ([g (~? f values)])
         (λ (_old self)
           (define x (g (send (~? ff self) (~? m get-value))))
           e ...))]))

(struct *none ())
(define none (*none))

(define (make-horizontal p) (new horizontal-pane% [parent p][alignment '(center center)]))
(define (make-vertical p) (new vertical-pane% [parent p][alignment '(center center)]))
