#lang racket/gui

(provide

 ;; SYNTAX
 #; (gui Title (state:id state:expr propagate:expr) gui-elements ...)

 gui)

(require (for-syntax syntax/parse))

(define-syntax (gui stx)
  (syntax-parse stx 
    [(_ Title (*count state0 f) visuals)

     #:with names (retrieve-ids #'visuals)
     
     #'(begin

         (define *count-field 0)

         (define-syntax *count
           (make-set!-transformer
            (lambda (stx) 
              (syntax-case stx ()
                [*count (identifier? #'*count) #'*count-field]
                [(set! *count e) #'(begin (set! *count-field e) (f *count-field))]))))

         (define frame (new frame% [label Title] [width 200] [height 77]))
         (horizontal-visuals names frame visuals)
         (send frame show #t))]))

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

(define-for-syntax (make-gui-elements frame gui-specs)
  (let loop ((gui-specs (syntax->list gui-specs)))
    (if (null? gui-specs)
        '()
        (syntax-parse (car gui-specs)
          [[#:id x:id gui-element:id . options]
           (cons #`[x (new gui-element [parent #,frame] . options)] (loop (cdr gui-specs)))]
          [[gui-element:id . options]
           (cons #`[y (new gui-element [parent #,frame] . options)] (loop (cdr gui-specs)))]))))
