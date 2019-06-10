#lang racket/gui

;; TODO
;; -- a macro to create #:change setters so that they retrieve values from fields etc.
;; -- can this be turned into #:change-if ? 

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
 gui

 stop
 many
 
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

(define-syntax (many stx)
  (raise-syntax-error #f "used out of context"))

(define-syntax (stop stx)
  (raise-syntax-error #f "used out of context"))

(require (for-template syntax/parse))

(define-syntax (define-getter/setter stx)
  (syntax-parse stx 
    [(_ (state state-field f) ...)
     #'(begin
         (define-syntax state
           (make-set!-transformer
            (lambda (stx) 
              (syntax-parse stx
                #:literals (stop many)
                [x:id #'state-field]
                [(set! x (stop e)) #'(set! state-field e)]
                [(set! x (many e))
                 #'(call-with-values
                    (λ () (apply values e))
                    (λ (y . r) (set! state-field y) (apply f state-field r)))]
                [(set! x e) #'(begin (set! state-field e) (f state-field))]))))
         ...)]))

(begin-for-syntax
  
  (define-syntax-class option
    #:description "name and value binding"
    (pattern (x:id e:expr)))

  (define-syntax-class gui-element
    #:description "gui element specification"
    (pattern ((~optional (~seq #:id x:id))
              widget:expr (~optional (~seq #:change s:id f:expr)) fv:option ...))
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
    [(_ p [(~optional (~seq #:id x:id)) w%:expr (~optional (~seq #:change s:id f:expr)) o:option ...])
     #`(begin
         (~? (~@ (define g f)) (~@))
         [define (~? x y) (new w% [parent p]
                               (~? (~@ [callback
                                        (λ (self evt)
                                          (define new (g s self))
                                          (unless (*none? new) (set! s new)))])
                                   (~@))
                               o ...)])]))

(provide with just)
(define ((just f) old _self) (f old))
(define-syntax (with stx)
  (syntax-parse stx
    [(_ x:id
        (~optional (~seq #:post f:expr))
        (~optional (~seq #:field ff:id))
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
