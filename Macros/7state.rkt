#lang racket

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
 #; (set! x (values e0 e ...))
 ;; evaluate e0 and e to a list of values, use e0's value as the new value
 ;; for x and propagate all of these values to the propagation function. 

 #; (set! x (stop e))
 ;; do not propagate this change to state variable x
 stop)


;; ---------------------------------------------------------------------------------------------------
(require (for-syntax syntax/parse))

;; ---------------------------------------------------------------------------------------------------
(define-syntax (define-state* stx)
  (syntax-parse stx
    [(_ (state:id state0:expr f:expr) ...) #'(begin (define-state state state0 f) ...)]))

(define-syntax (define-state stx)
  (syntax-parse stx
    [(_ state:id state0:expr f:expr)
     #'(begin
         (define g f)
         (define state-field state0)
         (define-getter/setter (state state-field g)))]))

(define-syntax (define-getter/setter stx)
  (syntax-parse stx 
    [(_ (state state-field f) ...)
     #'(begin (define-syntax state (generate-set-state #'state-field #'f)) ...)]))

(define-for-syntax (generate-set-state state-field f)
  (with-syntax ([state-field state-field][f f])
    (make-set!-transformer
     (lambda (stx) 
       (syntax-parse stx
         #:literals (stop values)
         [x:id #'state-field]
         [(set! x (stop e)) #'(set! state-field e)]
         [(set! x (values e0 e ...))
          #'(call-with-values
             (λ () (apply values (list e0 e ...)))
             (λ (y . r) (set! state-field y) (apply f state-field r)))]
         [(set! x e) #'(begin (set! state-field e) (f state-field))])))))

(define-syntax (stop stx) (raise-syntax-error #f "used out of context"))
