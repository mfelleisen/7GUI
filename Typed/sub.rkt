#lang typed/racket

(provide
 ;; type 
 MaybeString
 MaybeN
 MaybeInt

 ;; SYNTAX
 #;(define-sub-type define-type-class init:init-type ...)
 ;; defines the macro
 #;(define-type-class def-type:id #:minus-init (i:id ...) ctc:clause-type-clause ...)
 ;; which can (define-type def-type ...), a Class type with the given init fields init ...
 ;; plus the class type clauses ctc ...
 ;; minus the fields i ... specified via an initial #:minus-init (s ...) clause 

 define-sub-type)

;; ---------------------------------------------------------------------------------------------------
(require (for-syntax syntax/parse))
(require (for-syntax racket/function))
(require (for-syntax 7GUI/should-be-racket))

;; ---------------------------------------------------------------------------------------------------
(define-type MaybeString (U False String))

(define-type MaybeN (U Exact-Nonnegative-Integer False))

(define-type MaybeInt (U False Integer))

(begin-for-syntax
  (define-syntax-class init-type
    (pattern (name:id type (~optional #:optional))))

  (define-syntax-class class-type-clause
    #:literals (init init-field init-rest field augment)
    (pattern ((~optional (~or init init-field init-rest field augment)) (x:id t) ...)
             #:attr name #'(x ...)
             #:attr type #'(t ...))
    (pattern (y:id s)
             #:attr name #'(y)
             #:attr type #'(s))))
    
(define-syntax (define-sub-type stx)
  (syntax-parse stx
    [(_ def-type:id implements%:id p:init-type ...)
     #:with (t ...) (generate-temporaries #'(p ...))
     #`(begin
         (define-type t p.type) ...
         (define-syntax (def-type stx) (def-type-rhs stx #'implements% #'(p ...) #'(p.name ...))))]))

(define-for-syntax (def-type-rhs stx implements% init-parameters init-labels)
  (define inits0 (syntax->list init-parameters))
  (define label0 (map syntax-e (syntax->list init-labels)))
  (with-syntax ((stx stx) (implements% implements%))
    (syntax-parse #'stx
      [(_ name-of-type% (~optional (~seq #:minus-init (y:id ...))) ctc:class-type-clause ...)
       #:do ((define minuss (syntax->list #'(~? (y ...) ())))
             (define inits- (map syntax-e minuss))
             (define m* (for/first ((i inits-) (m minuss) #:unless (memf (curry eq? i) label0)) m)))
       #:fail-when m* (format "cannot subtract ~a from ~a" (syntax-e m*) label0)
       #:do ((define inits+ (for/list ([l label0][i inits0] #:unless (memf (curry eq? l) inits-)) i)))
       #`(define-type name-of-type% (Class #:implements implements% ctc ... (init #,@inits+)))])))