#lang racket/gui

(provide
 ;; SYNTAX
 #; (define-gui name:id Title:expr (state:id state:expr propagate:expr) gui-spec)
 ;; -- defines (define state state0) ...
 ;; -- re-defines set! for state ... so that any changes to state ... invoke propagate ...
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

;; ---------------------------------------------------------------------------------------------------
(begin-for-syntax
  
  (define-syntax-class field+value-expr
    #:description "name and value binding"
    (pattern (x:id e:expr)))

  (define-syntax-class gui-element
    #:description "gui element specification"
    (pattern (#:id x:id widget:expr fv:field+value-expr ...))
    (pattern (widget:expr fv:field+value-expr ...)))

  (define-syntax-class gui-spec
    #:description "gui specification"
    (pattern (ge:gui-element ...))))

(define-syntax (gui stx)
  (syntax-parse stx
    [(_ Title:expr {(state:id state0:expr f:expr) ...} visuals ...)
     #'(begin (define-gui frame Title {(state state0 f) ...} visuals ...) (send frame show #t))]))

(define-syntax (define-gui stx)
  (syntax-parse stx
    [(_ frame-name:id Title:expr {(state:id state0:expr f:expr) ...} visuals:gui-element ...)
     #:with (state-field ...) (generate-temporaries #'(state ...))
     #:with (g ...) (generate-temporaries #'(f ...))
     #'(begin
         (define-values (g ...) (values f ...))
         (define-values (state-field ...) (values state0 ...))
         (define-getter/setter (state state-field g) ...)
         (define frame-name (new frame% [label Title] [width 200] [height 77]))
         (define pane (new vertical-pane% [parent frame-name]))
         (setup-visuals pane (visuals))
         ...)]
    [(_ frame-name:id Title:expr {(state:id state0:expr f:expr) ...} visuals:gui-spec)
     #:with (state-field ...) (generate-temporaries #'(state ...))
     #'(begin
         (define-values (state-field ...) (values state0 ...))
         (define-getter/setter (state state-field f) ...)
         (define frame-name (new frame% [label Title] [width 200] [height 77]))
         (setup-visuals frame-name visuals))]))

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

(define-for-syntax (retrieve-ids stx)
  (let loop ([stx (syntax->list stx)])
    (if (null? stx) '()
        (syntax-parse (car stx)
          [(#:id name . stuff) (cons #'name (loop (cdr stx)))]
          [_ (loop (cdr stx))]))))

(define-syntax (setup-visuals stx)
  (syntax-parse stx 
    [(_ container (gui-specs ...))
     #:with (name ...) (retrieve-ids #'(gui-specs ...))
     #:do ((define horizontal #'(new  horizontal-pane% [parent container])))
     #:with pane #'pane
     #:with optionally-named-gui-elements (make-gui-elements #'pane #'(gui-specs ...))
     #`(define-values (name ...)
         (let ([pane #,horizontal])
           (let* optionally-named-gui-elements
             (values name ...))))]))

(define-for-syntax (make-gui-elements parent gui-specs)
  (let loop ((gui-specs (syntax->list gui-specs)))
    (if (null? gui-specs)
        '()
        (syntax-parse (car gui-specs)
          [[#:id x:id gui-element:id option:field+value-expr ...]
           (cons #`[x (new gui-element [parent #,parent] option ...)] (loop (cdr gui-specs)))]
          [[gui-element:id option:field+value-expr ...]
           (cons #`[y (new gui-element [parent #,parent] option ...)] (loop (cdr gui-specs)))]))))