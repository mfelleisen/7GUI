#lang racket/gui

(provide
 ;; SYNTAX
 #; (gui Title (state:id state:expr propagate:expr) gui-spec-elements ...)
 gui)

;; ---------------------------------------------------------------------------------------------------
(require (for-syntax syntax/parse))
(require (for-syntax racket/syntax))

;; ---------------------------------------------------------------------------------------------------
(define-syntax (gui stx)
  (syntax-parse stx 
    [(_ Title {(state:id state0:expr f:expr) ...} visuals)
     #:with names (retrieve-ids #'visuals)
     #:with (state-field ...) (generate-temporaries #'(state ...))
     #'(begin
         (define-values (state-field ...) (values state0 ...))
         (define-getter/setter (state state-field f) ...)
         (define frame (new frame% [label Title] [width 200] [height 77]))
         (horizontal-visuals names frame visuals)
         (send frame show #t))]))

(define-syntax (define-getter/setter stx)
  (syntax-parse stx 
    [(_ (state state-field f) ...)
     #'(begin
         (define-syntax state
           (make-set!-transformer
            (lambda (stx) 
              (syntax-case stx ()
                [state (identifier? #'state) #'state-field]
                [(set! state e) #'(begin (set! state-field e) (f state-field))]))))
         ...)]))

(define-for-syntax (retrieve-ids stx)
  (let loop ([stx (syntax->list stx)])
    (if (null? stx) '()
        (syntax-parse (car stx)
          [(#:id name . stuff) (cons #'name (loop (cdr stx)))]
          [_ (loop (cdr stx))]))))

(define-syntax (horizontal-visuals stx)
  (syntax-parse stx 
    [(_ (name ...) frame (gui-specs ...))
     #:do ((define horizontal #'(new horizontal-pane% [parent frame])))
     #:with pane #'pane
     #:with optionally-named-gui-elements (make-gui-elements #'pane #'(gui-specs ...))
     #`(define-values (name ...)
         (let ([pane #,horizontal])
           (let* optionally-named-gui-elements
             (values name ...))))]))

(begin-for-syntax
  (define-syntax-class field+value-expr
    #:description "name and value binding"
    (pattern (x:id e:expr))))

(define-for-syntax (make-gui-elements parent gui-specs)
  (let loop ((gui-specs (syntax->list gui-specs)))
    (if (null? gui-specs)
        '()
        (syntax-parse (car gui-specs)
          [[#:id x:id gui-element:id option:field+value-expr ...]
           (cons #`[x (new gui-element [parent #,parent] option ...)] (loop (cdr gui-specs)))]
          [[gui-element:id option:field+value-expr ...]
           (cons #`[y (new gui-element [parent #,parent] option ...)] (loop (cdr gui-specs)))]))))