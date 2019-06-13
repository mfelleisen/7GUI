#lang typed/racket

(provide Exp Content Ref Letter LETTERS string->exp* exp*->string depends-on evaluate)

(define-type Letter Char
  #;
  (U #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
     #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))

(define-type Exp False)
(define-type Ref (List Letter Index))
(define-type Content (Immutable-HashTable Ref Number))

(require/typed 7GUI/task-7-exp
               [LETTERS      String]
               [string->exp* (-> String Exp)]
               [exp*->string (-> Exp String)]
               [depends-on   (-> Exp (Listof Ref))]
               [evaluate     (-> Exp Content Integer)])
