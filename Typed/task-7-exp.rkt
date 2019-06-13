#lang typed/racket

(provide Exp Content Ref Letter LETTERS string->exp* exp*->string depends-on evaluate)

(define-type Letter Char
  #;
  (U #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
     #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))

(define-type Ref (List Letter Index))
(define-type Exp (U Integer Ref (List Symbol Exp Exp)))
(define-type Content (Immutable-HashTable Ref Integer))

(require/typed 7GUI/task-7-exp
               [LETTERS      String]
               [string->exp* (-> String Exp)]
               [exp*->string (-> Exp String)]
               [depends-on   (-> Exp (Setof Ref))]
               [evaluate     (-> Exp Content Integer)])
