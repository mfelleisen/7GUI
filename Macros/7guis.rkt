#lang racket/gui

(provide

 ;; SYNTAX
 #; (define-state state:id state0:expr propagate:expr)
 ;; -- defines (define state state0) ...
 ;; -- re-defines set! for state ... so that any changes to state ... invoke propagate ...

 define-state

 #; (define-state* (state:id state0:expr propagate:expr) ...)
 ;; (define-state state state0 propagate) ...
 define-state*
 
 ;; SYNTAX
 #; (define-gui name:id Title:expr gui-spec)
 ;; -- defines name to be a frame-based GUI according to the gui-spec
 ;; 
 #; {gui-spec    == (gui-element ...)
                 gui-element == (#:id x:id g:expr [l:id l0:expr] ...)
                 || (g:expr [l:id l0:expr] ...) }
 ;; a gui-element creates a widget using (new g [l l0] ...);
 ;; if an element comes with a #:id x, it is given the name x with the same scope as name
 define-gui

 #; (gui:id Title:expr (state:id state:expr propagate:expr) gui-spec)
 ;; like define-gui, but immediately shows the constructed frame 
 gui)

;; ---------------------------------------------------------------------------------------------------
(require (for-syntax syntax/parse))
(require (for-syntax racket/syntax))
(require (for-syntax racket/list))

;; ---------------------------------------------------------------------------------------------------
(define-syntax (define-state* stx)
  (syntax-parse stx
    [(_ (state:id state0:expr f:expr) ...) #'(begin (define-state state state0 f) ...)]))

(define-syntax (define-state stx)
  (syntax-parse stx
    [(_ state:id state0:expr f:expr)
     #:with (state-field) (generate-temporaries #'(state))
     #:with (g) (generate-temporaries #'(f))
     #'(begin
         (define g f)
         (define state-field state0)
         (define-getter/setter (state state-field g)))]))

(define-syntax (define-getter/setter stx)
  (syntax-parse stx 
    [(_ (state state-field f) ...)
     #'(begin
         (define-syntax state
           (make-set!-transformer
            (lambda (stx) 
              (syntax-case stx ()
                [x (identifier? #'x) #'state-field]
                [(set! x e) #'(begin (set! state-field e) (f state-field))]))))
         ...)]))

(begin-for-syntax
  
  (define-syntax-class field+value-expr
    #:description "name and value binding"
    (pattern (x:id e:expr)))

  (define-syntax-class gui-element
    #:description "gui element specification"
    (pattern (#:id x:id widget:expr fv:field+value-expr ...))
    (pattern (#:horizontal ge:gui-element ...))
    (pattern (widget:expr fv:field+value-expr ...)))

  (define-syntax-class gui-spec
    #:description "gui specification"
    (pattern (ge:gui-element ...))))

(define-syntax (gui stx)
  (syntax-parse stx
    [(_ (~optional (~seq #:frame f)) T Vs ...)
     #'(begin (define-gui frame (~? (~@ #:frame f) (~@)) T Vs ...) (send frame show #t))]))

(define-syntax (define-gui stx)
  (syntax-parse stx
    [(_ frame-name:id (~optional (~seq #:frame f%:expr)) Title:expr visuals:gui-element ...)
     #'(begin
         (define frame-name (new (~? f% frame%) [label Title] [width 200] [height 77]))
         (define pane (new vertical-pane% [parent frame-name]))
         (setup-visuals pane (visuals))
         ...)]

    [(_ frame-name:id (~optional (~seq #:frame f%:expr)) Title:expr visuals:gui-spec)
     #'(begin
         (define frame-name (new (~? f% frame%) [label Title] [width 200] [height 77]))
         (setup-visuals frame-name visuals))]))

(define-syntax (setup-visuals stx)
  (syntax-parse stx 
    [(_ container (gui-specs ...))
     #`(begin
         (define pane (new  horizontal-pane% [parent container]))
         (make-gui-element pane gui-specs) ...)]))

(define-syntax (make-gui-element stx)
  (syntax-parse stx
    [(_ p (#:horizontal b ...))
     #'(begin (define horizontal (new horizontal-pane% [parent p]))
              (make-gui-element horizontal b) ...)]
    [(_ p [#:id x:id gui-element:id option:field+value-expr ...])
     #`[define x (new gui-element [parent p] option ...)]]
    [(_ p [gui-element:id option:field+value-expr ...])
     #`[define y (new gui-element [parent p] option ...)]]))
