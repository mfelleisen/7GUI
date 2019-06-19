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
 #; (set! x (stop e))
 ;; do not propagate this change to state variable x
 stop

 ;; SYNTAX
 #; (set! x (values e))
 ;; evaluate e to a list of values (why does values not work?) 
 ;; the change propagater function must be of arity (length e) 
 
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

(define-syntax (stop stx) (raise-syntax-error #f "used out of context"))

(require (for-template syntax/parse))

(define-syntax (define-getter/setter stx)
  (syntax-parse stx 
    [(_ (state state-field f) ...)
     #'(begin
         (define-syntax state
           (make-set!-transformer
            (lambda (stx) 
              (syntax-parse stx
                #:literals (stop values)
                [x:id #'state-field]
                [(set! x (stop e)) #'(set! state-field e)]
                [(set! x (values e0 e (... ...)))
                 #'(call-with-values
                    (λ () (apply values (list e0 e (... ...))))
                    (λ (y . r) (set! state-field y) (apply f state-field r)))]
                [(set! x e) #'(begin (set! state-field e) (f state-field))]))))
         ...)]))

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
