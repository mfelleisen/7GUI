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
 )

;; ---------------------------------------------------------------------------------------------------
(require (for-syntax syntax/parse))
; (require (for-syntax racket/syntax))
; (require (for-syntax racket/list))

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

(define-state *x 0 (compose displayln list)) (set! *x 1) (set! *x (values 1 2 3))