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

(define-syntax (define-sub-type stx)
  (syntax-parse stx
    [(_ def-type-name:id implements%:id paras ...)
     #`(define-syntax (def-type-name stx) (generate-type-name-stx stx #'implements% #'(paras ...)))]))

(define-for-syntax (generate-type-name-stx stx implements% init-parameters)
  (with-syntax ((stx stx) (implements% implements%))
    (syntax-parse #'stx
      [(_ name-of-type% (~optional (~seq #:minus-init (y:id ...))) extra ...)
       #:do ((define inits (syntax->list init-parameters))
             (define labels (map (compose syntax-e car syntax-e) (syntax->list init-parameters)))
             (define yy (map syntax-e (syntax->list #'(~? (y ...) ()))))
             (when* (for*/first ((z yy) (yy (in-value z)) #:unless (memf (curry eq? yy) labels)) z)
                    => (lambda (y-not-in-labels)
                         (define fmt (format "cannot subtract ~a from ~a" y-not-in-labels labels))
                         (raise-syntax-error 'define-type-canvas fmt)))
             (define (in l) (memf (curry eq? l) yy))
             (define inits-included (for/list ([l labels][i inits] #:unless (in l)) i)))
       #`(define-type name-of-type%
           (Class #:implements implements%
                  ;; new methods:
                  extra ...
                  ;; revised inits:
                  (init #,@inits-included)))])))
