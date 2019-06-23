#lang typed/racket

(provide
 ;; SYNTAX
 #;(define-sub-type define-type-class (l:id  t:type ...))
 ;; defines a macro that can be used to define Class types with the given init fields
 ;; minus the fields s ... specified via an initial #:minus-init (s ...) clause 

 define-sub-type)

;; ---------------------------------------------------------------------------------------------------
(require (for-syntax syntax/parse))
(require (for-syntax racket/function))
(require (for-syntax 7GUI/should-be-racket))

;; ---------------------------------------------------------------------------------------------------
(begin-for-syntax
  (define-syntax-class class-type-clause
    (pattern ((~optional (~or init init-field init-rest field augment)) x:id t)))

  (define-syntax-class init-type
    (pattern (x:id t (~optional #:optional)))))

(define-syntax (define-sub-type stx)
  (syntax-parse stx
    [(_ def-type:id implements%:id p:init-type ...)
     #:with (t ...) (generate-temporaries #'(p ...))
     #`(begin
         (define-type t p.t) ...
         (define-syntax (def-type stx) (def-type-rhs stx #'implements% #'(p ...) #'(p.x ...))))]))

(define-for-syntax (def-type-rhs stx implements% init-parameters init-labels)
  (with-syntax ((stx stx) (implements% implements%))
    (syntax-parse #'stx
      [(_ name-of-type% (~optional (~seq #:minus-init (y:id ...))) ctc:class-type-clause ...)
       #:do ((define inits0 (syntax->list init-parameters))
             (define label0 (syntax->list init-labels))
             (define inits- (map syntax-e (syntax->list #'(~? (y ...) ()))))
             (when* (for*/first ((m inits-) #:unless (memf (curry eq? m) label0)) m)
                    => (lambda (y-not-in-labels)
                         (define fmt (format "cannot subtract ~a from ~a" y-not-in-labels label0))
                         (raise-syntax-error 'define-type-canvas fmt)))
             (define inits+ (for/list ([l label0][i inits0] #:unless (memf (curry eq? l) inits-)) i)))
       #`(define-type name-of-type% (Class #:implements implements% ctc ... (init #,@inits+)))])))